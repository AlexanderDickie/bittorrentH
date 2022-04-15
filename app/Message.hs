{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

module Message where

import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Set as Set
import qualified Data.Word as W8 (Word8)

import System.IO 

import qualified Network.Socket as S
import Network.Socket.ByteString 

import Utils
import Types 

{-
  functions to communicate with a peer
-}

blockSize :: Int
blockSize = 16000

-- **************** Handshake ******************

parseHandshake :: Int -> P.Parser Handshake
parseHandshake pstrLn = do
  identifier <- P.take pstrLn
  reserved <- P.take 8
  infoHash <- P.take 20
  peerId <- P.take 20
  return $ Handshake (C8.unpack identifier) (map (\x -> fromIntegral x :: Int ) (B.unpack reserved)) infoHash peerId

sendReceiveHandshake :: S.Socket -> B.ByteString -> B.ByteString -> B.ByteString -> IO Bool
sendReceiveHandshake s hs peerId infoHash= do
  send s hs  
  ln <- recv s 1 
  hs <- recv s $ bs2int ln + 48
  -- print hs
  case P.parseOnly (parseHandshake (bs2int ln)) hs of 
    Left err -> return False 
    Right (Handshake id res infoHash' peerId') -> return $ (peerId == peerId') && (infoHash == infoHash')

mkHandshake :: B.ByteString -> B.ByteString
mkHandshake infoHash' =  B.concat 
      [B.pack [19], 
      C8.pack  "BitTorrent protocol",
      B.pack $ replicate 8 0,
      infoHash',
      C8.pack "01234567890123456789"
      ]

-- ***************** receiving and parsing message ***************
parseMessage :: P.Parser Message
parseMessage = do
  
  ln <- P.take 4
  msg <- P.take $ endian2int ln 

  let msgId = fromIntegral (B.unpack msg !! 0) :: Int
  return $ case msgId of 
    0 -> Choke
    1 -> UnChoke
    2 -> Interested
    3 -> NotInterested
    4 -> Have (endian82int $ tail $ B.unpack msg)
    5 -> BitField $ Set.fromList $ set2bf 0 $ foldl (\ acc x -> acc ++ byte2bits x) [] (tail $ B.unpack msg) 
    6 -> do
      let body = tail $ B.unpack msg
      Request (endian82int (slice 0 3 body)) (endian82int (slice 4 7 body)) (endian82int (slice 8 11 body))
    7 -> do
      let body = tail $ B.unpack msg
      Piece (endian82int (slice 0 3 body)) (endian82int (slice 4 7 body)) 
        (B.pack (drop 8 body))
    _ -> Err "nope"


type ParseResult = P.IResult B.ByteString Message

receiveMessages' :: S.Socket -> [Message] -> ParseResult -> IO (Maybe (ParseResult, [Message]))
receiveMessages' s acc result = case result of 
    Partial _ -> do 
        bytes <- recv s 1024
        if bytes == B.pack [] then
          return $ Nothing
        else
          receiveMessages' s acc (feed result bytes)
    Done i r -> do
      if B.length i == 0 then do
        return $ Just (result, r:acc) else
        receiveMessages' s (r:acc) (P.parse parseMessage i)
    Fail{} -> return Nothing

receiveMessages :: S.Socket -> IO (Maybe (ParseResult, [Message]))
receiveMessages s = do
  msg <- recv s 1024
  -- print $ B.unpack msg
  if B.length msg == 0 then do
    S.close s 
    return Nothing
    else
    receiveMessages' s [] (P.parse parseMessage msg)
  

-- ***************** Downloading and uploading to/from peer ***************

sendMessages :: [Message] -> S.Socket -> IO ()
sendMessages ms s = do
  -- putStrLn $ "sending " ++ show ms
  let enMsgs = B.concat $ map encodeMessage ms
  send s enMsgs
  return ()  
sendMessage :: Message -> S.Socket -> IO ()
sendMessage ms = sendMessages [ms]

mkBlocksRequests :: NBlocks -> PieceLn -> PieceIdx -> Set.Set BlockIdx -> [Message]
mkBlocksRequests nb pLn pIdx blocks = Set.foldl (\ acc idx -> acc ++ [mkBlockRequest pLn nb pIdx idx]) [] blocks

mkBlockRequest :: PieceLn -> NBlocks -> PieceIdx -> BlockIdx -> Message
mkBlockRequest pl nb p b =
  if b == nb - 1 then Request p (b * blockSize) (pl - b * blockSize)
    else Request p (b * blockSize) blockSize

encodeMessage :: Message -> B.ByteString
encodeMessage m = 
    case m of 
      KeepAlive -> dec2endian 0
      Choke -> dec2endian 1 <> B.pack [0]
      UnChoke -> dec2endian 1 <> B.pack [1]
      Interested -> dec2endian 1 <> B.pack [2]
      NotInterested -> dec2endian 1 <> B.pack [3]
      Have index-> dec2endian 5 <> B.pack [4] <> B.pack [fromIntegral index::W8.Word8]
      BitField i -> dec2endian 5
      Request index begin ln -> dec2endian 13 <> B.pack [6] <> dec2endian index 
        <> dec2endian begin <> dec2endian ln

sendPiece :: Meta -> Message -> S.Socket -> IO ()
sendPiece lns (Request pIdx offset ln) s = do
  han <- openFile (fileName lns) ReadMode
  hSeek han AbsoluteSeek (fromIntegral (pIdx * (pieceLength lns) + offset)) 
  payLoad <- B.hGet han ln
  send s payLoad 
  return ()

data BlockInfo = BlockInfo {remainingBlocks :: Set.Set Int, blockMap :: Map.Map Int B.ByteString }

mkBlockInfo :: Int -> Int -> BlockInfo
mkBlockInfo pieceLn blockLn = 
  let nBlocks = ceiling ((fromIntegral pieceLn :: Float) / (fromIntegral blockLn::Float)) - 1
      blockInfo = blockInfo { remainingBlocks = Set.fromList [0..nBlocks],
              blockMap = Map.empty } :: BlockInfo
              in
                blockInfo

-- 