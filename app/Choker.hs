{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Choker where

import Data.Time 
import Data.List (sortBy)

import Control.Concurrent.STM
import Control.Monad.State
import Control.Concurrent

import System.Random (randomRIO)

import Types
import Message

{-
    Which peers to choke and which to unchoke?
    Implementaion of choke algorithm-
    every 10 sec we choose the top 3 uploaders of the peers that are interested, and unchoke them, 
        and choke the ones that are no longer top 3
    every 30 sec we optimistically unchoke one random peer out of the peers that are intersted (which aren't in top 3 uploaders)
        and choke previous optimistic peer
-}

import qualified Data.Set as Set

data ChokerSt = ChokerSt {
    peersT :: TVar AllPeers,
    topUploaders :: Set.Set PeerInfo,
    optimistic :: Maybe PeerInfo,
    orbits :: Int
}

newtype ChokerM a = Choker {unChokerM :: StateT ChokerSt IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadState ChokerSt)

-- runChokerM :: ChokerM a -> ChokerSt -> IO (a, ChokerSt)
runChokerM :: ChokerM b -> ChokerSt -> IO b
runChokerM choker st = fst <$> runStateT (unChokerM choker) st

choker :: ChokerM ()
choker = forever $ do
    liftIO $ threadDelay $ 10 * 10^6 -- 10 sec delay
    st <- get
    -- new top uploaders 
    allPeers <- liftIO $ readTVarIO $ peersT st
    newTopPeers <- liftIO $ topInterUploaders 3 $ allPeers

    -- choke the peers that no longer have top upload rates
    liftIO $ mapM chokePeer $ Set.toList $ Set.difference (topUploaders st) newTopPeers
    -- unchoke the peers that now have top upload rates, eg the new top peers
    liftIO $ mapM unchokePeer $ Set.toList $ Set.difference newTopPeers (topUploaders st)
    put st{
        topUploaders = newTopPeers,
        orbits = ((orbits st) + 1) `mod` 3
    }
    
    -- every 30s find new optimistic peer
    if orbits st == 0 then do -- every 30 sec unchoke a random, interested, non top 3 peer
        st <- get
        let curOp' = optimistic st
        newOp' <- liftIO $ findOpInterPeer (Set.difference allPeers newTopPeers) -- choose optimistic peer
        put st{optimistic = newOp'}
        
    -- depending on possibliites for new/old optimistic peer, choke the old and unchoke the new
        case newOp' of 
            Nothing -> case curOp' of 
                Nothing -> return ()
                Just curOp -> liftIO $ chokePeer curOp 
            Just newOp -> case curOp' of 
                Nothing -> liftIO $ unchokePeer newOp 
                Just curOp -> 
                    if curOp == newOp 
                        then return ()
                        else (liftIO $ chokePeer newOp) >> (liftIO $ unchokePeer newOp)

    else return ()

-- change env, send msg to peer
chokePeer :: PeerInfo -> IO ()
chokePeer pInfo = (atomically $ writeTVar (partnerChokedT pInfo) True) >> sendMessage Choke (socket pInfo)
unchokePeer:: PeerInfo -> IO ()
unchokePeer pInfo = (atomically $ writeTVar (partnerChokedT pInfo) False) >> sendMessage UnChoke (socket pInfo)

-- find the top n uploaders of the peers that are intersted
topInterUploaders :: Int -> Set.Set PeerInfo -> IO (Set.Set PeerInfo)
topInterUploaders n peers = do
    inter <- findInterPeers (Set.toList peers)
    findTopUploaders n inter
-- choose randomly one peer thats interested
findOpInterPeer :: Set.Set PeerInfo -> IO (Maybe PeerInfo)
findOpInterPeer peers = do
    intPeers <- findInterPeers $ Set.toList peers
    rInt <- randomRIO (0, length intPeers-1)
    if length intPeers == 0 
        then return Nothing
        else return $ Just $ intPeers !! rInt

-- *********** Helper Functions

uploadRate :: PeerInfo -> IO (Float, PeerInfo)
uploadRate pInfo = do
    curTime <- getCurrentTime
    let elapsed = diffUTCTime (startTime pInfo) curTime
    uploaded <- readTVarIO $ uploadedT pInfo
    return $ ((fromIntegral uploaded :: Float) / (realToFrac elapsed :: Float), pInfo)

-- finds the top n uploaders from a list of peers
findTopUploaders :: Int -> [PeerInfo] -> IO (Set.Set PeerInfo)
findTopUploaders n peers = do
    pInfoRates <- mapM uploadRate peers
    return $ Set.fromList $ map (\(r,pInfo) -> pInfo) (take n $ sortBy (\(r1,_) (r2,_) -> compare r1 r2 ) pInfoRates)

findInterPeers :: [PeerInfo] -> IO [PeerInfo]
findInterPeers peers = filterM isInterested peers 
    where 
        isInterested :: PeerInfo -> IO Bool
        isInterested peer = do
            interested <- readTVarIO $ partnerInterestedT peer
            return interested




