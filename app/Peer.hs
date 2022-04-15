{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Peer where

import qualified Data.ByteString as B
import qualified Data.Map as Map 
import qualified Data.Set as Set

import System.IO
import System.Random (randomRIO)
import Data.Time
import Control.Exception

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.STM.TChan
import Control.Concurrent.Async
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Lens

import qualified Network.Socket as S
import Network.Socket.ByteString as BS

import Message
import Types
import PieceSelector

{-
  Functions existing to communicate with the peer
-}

newtype PeerM a = PeerM {unPeerM :: ReaderT Env IO a}
  deriving (Functor, Monad, Applicative, MonadReader Env, MonadIO)

runPeerM :: PeerM a -> Env -> IO a
runPeerM peer env = runReaderT (unPeerM peer) env

-- concurrently, given connection info, create peerInfo, add to all peers then start the peer logic
startPeer ::  TVar AllPeers ->  WriterChan -> WriterMapT -> Meta -> Global -> (PeerId, Ip, Port) -> IO ThreadId
startPeer allPeersT wChan wMapT meta global connection = forkIO $ do
  -- create peerInfo, add to all peers
  pInfo <- mkPeerInfo connection
  writePeerInfo pInfo allPeersT

  -- initilize the peer's Env
  localDataChan <- newChan
  dlThreadT <- newTVarIO Nothing

  amInterestedT <- newTVarIO False
  amChokedT <- newTVarIO True

  let 
    env = Env {
      meta = meta,
      global = global,
      writerMapT = wMapT,
      writerChan = wChan,
      
      amInterestedT = amInterestedT, 
      amChokedT = amChokedT,
      
      peerInfo = pInfo,
      dlThreadT = dlThreadT,
      dataChan = localDataChan
    }
  -- start peer logic, in event of peer exception, remove peer from all peers structure and close the socket
  -- finally (runPeerM peer env) $ (removePeer pInfo allPeersT (rfInfoT global)) >> S.close (socket pInfo) >> print "lost a peer"
  catch (runPeerM peer env) 
    (\ (e :: SomeException) -> (removePeer pInfo allPeersT (rfInfoT global)) >> S.close (socket pInfo))
 

-- ********* Start Peer helper functions ************

-- from connection info, initiliaze a socket create peer info
mkPeerInfo :: (PeerId, Ip, Port) -> IO PeerInfo
mkPeerInfo (id, ip, port) = do
  partnerInterestedT <- newTVarIO False
  partnerChokedT <- newTVarIO True

  uploadedT <- newTVarIO 0
  downloadedT <- newTVarIO 0

  curTime <- getCurrentTime

  peerBitfieldT <- newTVarIO Set.empty
  --  connect to the socket
  addrInfo <- S.getAddrInfo Nothing (Just ip) (Just $ show $ port)
  let serverAddr = head addrInfo
  sock <- S.socket (S.addrFamily serverAddr) S.Stream S.defaultProtocol
  S.connect sock (S.addrAddress serverAddr)

  return $ PeerInfo {
              peerId = id,
              ip = ip, 
              port = port,

              peerBitfieldT = peerBitfieldT,
              socket = sock, -- need in info for choker to access

              partnerInterestedT = partnerInterestedT,
              partnerChokedT = partnerChokedT,
              
              startTime = curTime,
              uploadedT = uploadedT,
              downloadedT = downloadedT
            }

-- write peerInfo 
writePeerInfo :: PeerInfo -> TVar (Set.Set PeerInfo) ->  IO ()
writePeerInfo pInfo allPeersT = do
  -- set a timeout here
  ap <- atomically $ do 
    allPeers <- readTVar allPeersT
    writeTVar allPeersT $ Set.insert pInfo allPeers
    return allPeers
  return ()

removePeer :: PeerInfo -> TVar AllPeers -> TVar RFInfo -> IO ()
removePeer pInfo allPeersT rfInfoT = do
  
  atomically $ do -- remove from allPeers 
    allPeers <- readTVar allPeersT
    writeTVar allPeersT (Set.delete pInfo allPeers)

  atomically $ do -- remove bitfield from rfInfo
    peerBitfield <- readTVar (peerBitfieldT pInfo)
    bitfieldChange False peerBitfield rfInfoT
  return ()

-- ***************** Peer Logic ***************

-- exchange handshake with peer, and if valid start peer logic
peer :: PeerM ()
peer = do
  env <- ask
  let 
    sock = socket $ peerInfo env
    infoHash' = infoHash $ meta env
    handshake = mkHandshake infoHash'
  
  hsValid <- liftIO $ sendReceiveHandshake sock handshake (peerId $ peerInfo env) infoHash'
  if not hsValid then liftIO $ S.close sock  else do
    forever receiveReact -- start logic

-- receive message from peer and react to it
receiveReact :: PeerM ()
receiveReact = do
    env <- ask
    let sock = socket $ peerInfo env
    received <- liftIO $ receiveMessages sock
    case received of 
      Just (_, msgs) -> mapM_ reactMessage msgs
      Nothing -> return ()

-- react to message from peer
reactMessage :: Message -> PeerM ()
reactMessage message = do
  env <- ask
  let sock = socket $ peerInfo env
  case message of 
    Choke -> do
      liftIO $ atomically $ writeTVar (amChokedT env) True
      maybeThread <- liftIO $ readTVarIO $ dlThreadT env
      case maybeThread of 
        Nothing -> return ()
        Just threadId -> liftIO $ cancel threadId
      -- changed state to Choke, is download in pregress.
    UnChoke -> do
      maybeThread <- liftIO $ readTVarIO $ dlThreadT env
      case maybeThread of 
        Nothing -> do
          thread <- liftIO $ async $ dl env
          liftIO $ atomically $ writeTVar (dlThreadT env) (Just thread)
        Just threadId -> return ()
    Interested -> (liftIO $ atomically $ writeTVar (partnerInterestedT $ peerInfo env) True )>> (liftIO $ print "someone interested")
    NotInterested -> liftIO $ atomically $ writeTVar (partnerInterestedT $ peerInfo env) False
    val@(Piece a b c) -> do 
      liftIO $ writeChan (dataChan env) val
    BitField bf -> do 
      liftIO $ atomically $ writeTVar (peerBitfieldT $ peerInfo env) bf -- put in peers env
      
      imUseful <- liftIO $ atomically $ bitfieldChange True bf (rfInfoT $ global env) -- add to rarity map in global 
      -- liftIO $ putStrLn $  "bf" ++ show (socket $ peerInfo env)
      if imUseful then do -- send interested message
        liftIO $ atomically $ writeTVar (amInterestedT env) True
        liftIO $ sendMessage Interested sock
        else return ()  
    req@(Request pIdx begin ln) -> do
      partnerChoked <- liftIO $ readTVarIO (partnerChokedT $ peerInfo env) 
      case partnerChoked of 
        False -> return () 
        True -> do
          writerMap <- liftIO $ readTVarIO (writerMapT env)
          case Map.lookup pIdx writerMap of 
            Just _ -> return () -- pIdx hasnt been removed from writerMap, hence we don't have it
            Nothing -> do -- we have the piece, so check request is legit then send
              let pieceEnd = (pieceLength (meta env)) - 1
              if begin < 0 || begin > pieceEnd || (begin + ln - 1)> pieceEnd then return () -- invalid request
              else do
                liftIO $ sendPiece (meta env) req sock  -- valid request so send piece
    Have pIdx -> liftIO $ atomically $ do -- update peers bitfield
      bf <- readTVar (peerBitfieldT $ peerInfo env)
      writeTVar (peerBitfieldT $ peerInfo env) (Set.insert pIdx bf)


-- ********************** Download **********************
-- Attempt to download a piece from a

dl :: Env -> IO ()
dl env =  do
  randomInt <- randomRIO (0, (nPieces $ meta env))
  bitfield <- readTVarIO $ peerBitfieldT $ peerInfo env
  -- chosenPieceIdx <- atomically $ choosePiece randomInt bitfield (rfInfoT $ global env)
  let chosenPieceIdx = Just randomInt
  -- putStrLn $ "idx" ++ show (socket $ peerInfo env) ++ " " ++ show chosenPieceIdx
  case chosenPieceIdx of 
    Nothing -> do
      atomically $ writeTVar (amInterestedT env) False
      sendMessage NotInterested (socket $ peerInfo env)
    Just pIdx -> case Map.lookup pIdx (pieces $ global env) of -- find piece in global pieces
        Just pieceT -> dlBlocks env pIdx pieceT

-- given a piece, pick off max 10 blocks and attempt to download them
dlBlocks :: Env -> PieceIdx -> TVar Piece -> IO ()
dlBlocks env pIdx pieceT = do
  chosenBlocks <- atomically $ do 
    piece <- readTVar pieceT
    let 
      chosenBlocks' = Set.take 10 piece
      newPiece = Set.difference piece chosenBlocks'
    writeTVar pieceT (Set.difference newPiece chosenBlocks') -- remove blocks from piece
    return chosenBlocks'

  if Set.null chosenBlocks then do
    {-
      Here instead of removing the piece from inprogress and remaining pieces, could remove only from inprogress. 
      This naturally implements end-game. But need to integrate cancel messages
    
    -}
    atomically $ do -- remove pIdx from remainingpieces and inProgress
        rfInfo <- readTVar $ rfInfoT $ global env
        writeTVar (rfInfoT $ global env) rfInfo{
          inProgress = Set.delete pIdx (inProgress rfInfo),
          remainingPieces = Set.delete pIdx (remainingPieces rfInfo)
          }
    dl env -- restart download process
    else do -- download blocks from the piece
      reqPass env pIdx pieceT chosenBlocks `catch` (\(err :: SomeException) -> do 
          -- if we fail return pIdx to remaining and inProgress, and put the blocks back onto the piece
            atomically $ do 
              rfInfo <- readTVar $ rfInfoT $ global env
              writeTVar (rfInfoT $ global env) rfInfo{
                inProgress = Set.insert pIdx (inProgress rfInfo),
                inProgressMp = Map.adjust (+ (-1)) pIdx (inProgressMp rfInfo),
                remainingPieces = Set.insert pIdx (remainingPieces rfInfo)
                }
            atomically $ do 
              remainingPiece <- readTVar pieceT 
              writeTVar pieceT (Set.union remainingPiece chosenBlocks))
      dlBlocks env pIdx pieceT -- continue trying to download blocks from the piece
      

-- request the chosen blocks, then pass to writerChan
reqPass :: Env -> PieceIdx -> TVar Piece -> Set.Set BlockIdx -> IO ()
reqPass env pIdx pT blocks = do
  liftIO $ sendMessages 
    (mkBlocksRequests (nBlocks $ meta env) (pieceLength $ meta env) pIdx blocks) (socket $ peerInfo env)
  readPass (Set.size blocks) (dataChan env) (writerChan env)
  
{-
  read n blocks from our dataChan, and send them to the writerChan
  maybe add check to see we received what we requested
-}
readPass :: Int -> DataChan -> WriterChan ->  IO ()
readPass 0 dChan wChan = return ()
readPass i dChan wChan = do
  -- putStrLn $  "peer received" 
  msg <- readChan dChan
  case msg of 
    block@(Piece pIdx offset bs) -> do
      -- putStrLn $ show pIdx ++ show offset
      writeChan wChan block
      readPass (i-1) dChan wChan
    -- _ -> throwError
    










-- misc...
-- receiving peers
  -- forkIO $ do 
  --   serve HostAny "51413" $ \ (sock, addr) -> do
  --       putStrLn $ "connected to  ! " ++ show addr
        -- forever $ do
            -- msg <- SB.recv sock 1024
            -- SB.send sock msg


-- dl :: Env -> IO ()
-- dl env = forever $ do
--   -- print "first download process"
--   -- choose piece to donwload from
--   rarity <- readTVarIO (rarityT $ global env)
--   peerBitfield <- readTVarIO $ peerBitfieldT env
--   -- print $ inProgress rarity
--   -- print rarity
--   -- print $ Set.size peerBitfield
--   chosenPIdx <- choosePiece peerBitfield rarity
--   print "chosen a piece"

--   case chosenPIdx of 
--     Just pIdx -> do
--       case Map.lookup pIdx (pieces $ global env) of -- find piece in global pieces
--         Just pieceT -> dlBlocks env pIdx pieceT
--     Nothing -> do
--       sock <- readTVarIO (socketT env)
--       sendMessage NotInterested sock
--       atomically $ writeTVar (interestedT env) False

-- -- mask here for async exceptions
-- dlBlocks :: Env -> PieceIdx -> TVar Piece -> IO ()
-- dlBlocks env pIdx pieceT = do
--   -- remaining <- readTVarIO $ remainingPiecesT $ global env
--   -- print $ Set.size remaining

--   -- choose blocks to download  
--   -- print "downloading blocks"
--   chosenBlocks <- atomically $ do -- remove 10 blocks from the piece
--     piece <- readTVar pieceT
--     let 
--       chosenBlocks' = Set.take 10 piece
--       newPiece = Set.difference piece chosenBlocks'
--     writeTVar pieceT (Set.difference newPiece chosenBlocks') -- remove blocks from piece
    
--     if Set.null newPiece then do -- if piece is now null: 
--       removeGlobal (global env) pIdx -- remove piece from global structures- remainingPieces and rarity
--       return chosenBlocks'
--       else return chosenBlocks' 
--   -- download the chosen blocks
--   if Set.null chosenBlocks then dl env -- no blocks to download so restart dl process
--     else do -- download chosen blocks
--       reqPass env pIdx pieceT chosenBlocks 
--         `catch` (\(err :: SomeException) -> atomically $ do
--         -- if we fail when downloading the blocks:
--           remainingPiece <- readTVar pieceT 
--           writeTVar pieceT (Set.union remainingPiece chosenBlocks) -- return the blocks onto the piece
          
--           remainingPieces <- readTVar (remainingPiecesT $ global env) -- if the piece is missing from global pieces and rarity, add it back by:
--           if not $ Set.member pIdx remainingPieces then do 
--             addGlobal (global env) pIdx -- add to global remaining structures, and to rarity
--             else return ())



-- ********************** **********************




