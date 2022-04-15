module Meta where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as C8 (unpack, pack)
import Data.ByteString.Lazy as L (toStrict)
import Data.Maybe (fromJust)

import Control.Concurrent.STM 
import Control.Lens hiding (List)

import qualified Network.HTTP.Conduit as HTTP
import Crypto.Hash.SHA1 (hash)

import Message
import Utils
import Types
import Bencode

{-
  Communication and parsing from the tracker
-}

-- ***************** Meta + Global
-- initilize the meta structure when starting the program
mkMeta :: TorrentFile -> Meta
mkMeta torFile  = do
  let 
    torrentLength = fromJust $ torFile ^? bkey ( "info" ). bkey ( "length") . bnumber
    pieceLength = fromJust $ torFile ^? bkey ( "info") . bkey ( "piece length") . bnumber
    blockLength = 16000

  Meta {
    fileName = C8.unpack $ torFile ^. bkey ( "info") . bkey ( "name") . bstring,

    torrentLength = torrentLength,

    pieceLength = pieceLength,
    blockLength = blockLength,

    nBlocks = ceiling $ (fromIntegral pieceLength / fromIntegral blockLength) ,
    nPieces = ceiling $ (fromIntegral torrentLength/ fromIntegral (pieceLength)),

    infoHash =  hash $ mkInfo $ torFile
  } 

-- initilize global structure when starting the program
mkBlankGlobal :: Meta -> STM Global
mkBlankGlobal meta = do
    rpT <- newTVar $ Set.fromList [0..((nPieces meta)-1)] -- initialize a set containing all piece idx's

    pieces' <- mapM newTVar $ replicate (nPieces meta) (Set.fromList [0..(nBlocks meta-1)] :: Piece) 
    let 
        piecesT = Map.fromList $ zip [0..((nPieces meta)-1)] pieces' -- initialize map from all pieceidx's to full set of blocks
        rfInfo = RFInfo {
            remainingPieces = Set.fromList [0..(nPieces meta)-1],
            inProgress = Set.empty,
            inProgressMp = Map.fromList $ zip [0..(nPieces meta)-1] (replicate (nPieces meta) 0),
            rarityMap = Map.fromList $ zip [0..(nPieces meta)-1] (replicate (nPieces meta) 0)
            } 
    rfInfoT <- newTVar rfInfo
    
    return $ Global {
                pieces = piecesT,
                rfInfoT = rfInfoT
            }

-- ************** Peers 
-- given a blob of peers from tracker response, add these peers to our existing group of peers
findTrackerPeers ::  TrackerResponse -> [(PeerId, Ip, Port)]
findTrackerPeers tResp  = parsePeers tResp
  where 
    parsePeers :: TrackerResponse -> [(PeerId, Ip, Port)]
    parsePeers tResp = zip3 ((tResp ^.. bkey "peers" . blist. bkey "peer id" . bstring))
      (map C8.unpack $ (tResp ^.. bkey "peers" . blist. bkey "ip" . bstring))
      (tResp ^.. bkey "peers" . blist. bkey "port" . bnumber)

-- *********** Tracker 
mkTrackerInfo :: TrackerResponse -> TrackerInfo
mkTrackerInfo tResp = TrackerInfo {
  interval =  fromJust $ tResp ^? bkey ( "interval")  . bnumber,

  nSeeders =  fromJust $ tResp ^? bkey ( "complete") . bnumber,
  nLeechers = fromJust $ tResp ^? bkey ( "incomplete") . bnumber
}

getTResponse :: String -> IO Ben
getTResponse fileName = do
  file <- B.readFile fileName
  let tordict = readExpr file
  req <- sendGet tordict
  return $ req

mkInfo :: Ben -> B.ByteString
mkInfo ben = serialize $ fromJust $ ben ^? bkey ( "info")

-- send http get request to the tracker
sendGet :: Ben -> IO Ben
sendGet torDict = do
  req <- HTTP.simpleHttp url
  return $ readExpr $ toStrict req
  where
    url = concat [
      C8.unpack ( (torDict ^. (bkey) ( "announce") . bstring)),
      "?" ,
      "info_hash=" ++ (toUrl . hash . mkInfo) torDict  ,
      "&",
      "peer_id=" ++ "01234567890123456789",
      "&",
      "port=" ++ "51413",
      "&",
      "uploaded=" ++ "0",
      "&",
      "downloaded=" ++ "0",
      "&",
      "left=" ++ "50000"
      ]      

-- ********** hash helpers
splitHashs :: B.ByteString -> [B.ByteString] -> [B.ByteString]
splitHashs b acc = case B.length b of 
    0 -> acc
    l -> splitHashs (B.drop 20 b) (acc <> [B.take 20 b])

mkHashs :: TorrentFile -> Meta ->  PieceHashs 
mkHashs torFile meta = 
  let Just all = torFile ^? bkey "info" . bkey "pieces" . bstring in
  Map.fromList $ zip [0..(nPieces meta - 1)] (splitHashs all [])

