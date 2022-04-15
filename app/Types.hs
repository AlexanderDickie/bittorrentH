module Types where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.ByteString as B 

import qualified Network.Socket as S
import Data.Time

{-
  global types used throughout the project

-}

type AllPeers = Set.Set PeerInfo 

type TrackerResponse = Ben
type TorrentFile = Ben

type Ip = String
type Port = Int

type PieceIdx = Int
type BlockIdx = Int
type Piece = Set.Set BlockIdx
type Pieces = Map.Map PieceIdx (TVar Piece)
type PieceMap = Map.Map Int Piece
type PieceLn = Int
type NBlocks = Int
type Bitfield = Set.Set PieceIdx

type Hash = B.ByteString
type PieceHashs = Map.Map PieceIdx Hash 
type Rareness = Int

type DataChan = Chan Message
type WriterChan = Chan Message
type WriterMapT = TVar PieceMap

-- *************** Meta Structures *************

data TrackerInfo = TrackerInfo {
  interval :: Int,
  
  nSeeders :: Int,
  nLeechers :: Int
} deriving (Show)


data Meta = Meta {
  fileName :: String,
  torrentLength :: Int,

  nPieces :: Int,
  nBlocks :: Int,

  pieceLength :: Int,
  blockLength :: Int,

  infoHash :: B.ByteString
} deriving (Show)

-- *************** Global Enviroment
data Global = Global {
    pieces :: Map.Map PieceIdx (TVar Piece),
    rfInfoT :: TVar RFInfo
}

data RFInfo = RFInfo {
  remainingPieces :: Set.Set PieceIdx,
  inProgress :: Set.Set PieceIdx,
  inProgressMp :: Map.Map PieceIdx Int,
  rarityMap :: Map.Map PieceIdx Int
} deriving (Show)

-- *************** Peer Enviroment
data Env = Env {
  meta :: Meta,
  global :: Global,

  writerMapT :: WriterMapT,
  writerChan :: Chan Message,
  
  amInterestedT :: TVar Bool, 
  amChokedT :: TVar Bool,

  peerInfo :: PeerInfo,
  dlThreadT :: TVar (Maybe (Async ())),
  dataChan :: Chan Message
  }

data PeerInfo = PeerInfo {
  peerId :: B.ByteString,
  ip :: Ip, 
  port :: Port,

  peerBitfieldT :: TVar Bitfield,
  socket :: S.Socket , -- need in info for choker to access

  partnerInterestedT :: TVar Bool,
  partnerChokedT :: TVar Bool,
  
  startTime :: UTCTime,
  uploadedT :: TVar Int,
  downloadedT :: TVar Int
} deriving (Eq)

instance Ord PeerInfo where -- needed for set.delete, and remove etc, what is important is if id==id
  compare p1 p2 = 
    case (peerId p1) == (peerId p2) of 
      True -> EQ
      False -> LT 


instance Show PeerInfo where show = showPeerInfo

showPeerInfo :: PeerInfo -> String
showPeerInfo p = ip p ++ " " ++  (show $ port p) ++ " " ++ (show $ peerId p)

-- *************** Handshake
type Identifier = String
type Reserved = [Int]
type InfoHash = B.ByteString 
type PeerId = B.ByteString

data Handshake = Handshake Identifier Reserved InfoHash PeerId

-- *************** Message 

type Offset = Int
type RequestLn = Int

data Message =
  Err String |
  KeepAlive |
  Choke | 
  UnChoke | 
  Interested | 
  NotInterested | 
  Have PieceIdx |
  BitField Bitfield |
  Request PieceIdx Offset RequestLn |
  Piece PieceIdx Offset B.ByteString |
  Cancel Int |
  Port Int 
  deriving (Show)

-- ************** Bencode

type Bmap = Map.Map Ben Ben

data Ben = 
  String B.ByteString |
  Integer Int |
  Dict Bmap |
  List [Ben]
  deriving (Eq, Ord)