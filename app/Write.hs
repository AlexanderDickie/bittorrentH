{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Write where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString as B
import Data.Maybe (fromJust)

import System.IO 
import System.ProgressBar

import Control.Monad.State
import Control.Concurrent.Chan
import Control.Concurrent.STM

import Crypto.Hash.SHA1

import Message
import Meta
import Types

{-
    Functions involving writing pieces of the wanted file to disk, and checking the piece.
-}

type WriterStT = TVar PieceMap

writer :: Global -> TorrentFile -> Meta -> DataChan -> WriterMapT -> TVar AllPeers ->  IO ()
writer global torFile meta dchan wMapT allPeersT = do
    let style = defStyle{ 
                    stylePostfix = exact
                }
    pb <- liftIO $ newProgressBar style 10 (Progress 0 (nPieces meta) ())
    mkBlankFile meta
    han <- openFile (fileName meta) ReadWriteMode
    
    forever $ writeBlock global meta (mkHashs torFile meta) han dchan wMapT allPeersT pb
    
mkBlankFile :: Meta -> IO ()
mkBlankFile m = do
    han <- openFile (fileName m) WriteMode
    hSetFileSize han (fromIntegral $ torrentLength m)
    hClose han

{-

    the peer that finishes downloading a piece removes the pieces from remainingPieces and inProgress

    when the writer writes a partial piece, it removes the partial blocks from the piece map
    when the writer checks the hash:
        if good: it removes the piece from its piece map
        if bad: it puts the piece back into remaining pieces and puts full piece back into pieces
            and puts full piece into its own piece map
-}

writeBlock :: Global -> Meta -> PieceHashs -> Handle -> DataChan -> WriterMapT -> TVar AllPeers -> ProgressBar () -> IO ()
writeBlock global meta hs  han dChan wMapT allPeersT pb = do
    msg <- liftIO $ readChan dChan
    case msg of 
        Piece pIdx offset bs -> do
            wMap <- readTVarIO wMapT
            case Map.lookup pIdx wMap of -- check if piece exists in writer's own Piece set
                Nothing -> do
                    return () 
                Just piece -> do 
                    let 
                        blockIdx = offset `div` (blockLength meta)
                        newLocalPiece = Set.delete blockIdx piece -- updated piece (minus the written block)
                    
                    hSeek han AbsoluteSeek (fromIntegral (pIdx * (pieceLength meta) + offset)) 
                    B.hPut han bs -- write block to disk

                    if not $ Set.null newLocalPiece 
                        then do
                            atomically $ writeTVar wMapT (Map.insert pIdx newLocalPiece wMap)  -- piece is partially complete, update writerMap
                        else do -- the piece is now complete
                            hSeek han AbsoluteSeek (fromIntegral (pIdx * (pieceLength meta))) -- get the whole piece from disk
                            wholePiece <- B.hGet han (pieceLength meta)
                            
                            let myHash = hash $ wholePiece
                            if checkHash myHash hs pIdx 
                                then do -- good hash, remove the piece from writermap, send have message to all peers
                                    ln <- atomically $ do -- remove from writermap
                                        wMap <- readTVar wMapT
                                        writeTVar wMapT $ Map.delete pIdx wMap
                                        return $ Map.size wMap
                                    
                                    incProgress pb 1

                                    -- send have message to all peers 
                                    -- allPeers <- readTVarIO allPeersT
                                    -- mapM_ (sendMessage (Have pIdx)) (map socket (Set.toList allPeers))
                                    
                                else do -- bad hash, so put a full piece into pieces structure and writerMap:
                                    print "bad hash"
                                    atomically $ do writeTVar wMapT (Map.insert pIdx (Set.fromList [0..(nBlocks meta -1)]) wMap) -- full piece onto writerMap
                                    atomically $ do -- full piece onto global pieces
                                        let pieceT = fromJust $ Map.lookup pIdx (pieces global)
                                        writeTVar pieceT (Set.fromList [0..(nBlocks meta -1)])
                                    atomically $ do -- pIdx back into remainingPieces
                                        rfInfo <- readTVar (rfInfoT global)
                                        writeTVar (rfInfoT global) rfInfo{remainingPieces = Set.insert pIdx (remainingPieces rfInfo)}

checkHash :: Hash -> PieceHashs -> Int -> Bool 
checkHash h hs i = h == fromJust (Map.lookup i hs)


                
