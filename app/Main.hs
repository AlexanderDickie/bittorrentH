{-# LANGUAGE TemplateHaskell, RankNTypes, GeneralizedNewtypeDeriving, RecordWildCards #-}

module Main where

import qualified Data.ByteString as B 
import qualified Data.Map as Map 
import qualified Data.Set as Set

-- import Network.Simple.TCP (serve)

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

import Bencode 
import Utils
import Peer
import Meta
import Write
import Types
import Choker

{-
    initilize structures and start the various parts of the client
-}

torrentName :: String
torrentName = "ubuntu.torrent"

main :: IO ()
main = do
  -- get Tracker response
  file <- B.readFile $ torrentName 
  let torFile = readExpr file
  tResponse <- getTResponse $ torrentName 

  -- create meta and global structures
  let meta = mkMeta torFile
  global <- atomically  $ mkBlankGlobal meta
  allPeersT <- newTVarIO Set.empty

  -- start writer
  writerChan <- newChan 
  -- create a map from pieces idxs to full pieces 
  let 
    writerSt = Map.fromList $ zip [0..(nPieces meta - 1)] 
        (replicate (nPieces meta) (Set.fromList [0..(nBlocks meta-1)] :: Piece))
  writerMapT <- newTVarIO writerSt
  writerAsync <- async $ writer global torFile meta writerChan writerMapT allPeersT

  -- start choker
  let chokerSt = ChokerSt {
    peersT = allPeersT,
    topUploaders = Set.empty,
    optimistic = Nothing,
    orbits = -1
  }
  chokerAsync <- async $ runChokerM choker chokerSt

  -- connect to the peers given in the tracker response 
  let trackerPeers = findTrackerPeers tResponse
  mapM_ (startPeer allPeersT writerChan writerMapT meta global) trackerPeers

  waitAnyCatchCancel [writerAsync, chokerAsync] -- wait for writer to finish, catch a fatal error from writer/choker

  return ()



  
