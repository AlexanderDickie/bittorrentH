{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module PieceSelector where

import Utils
import Types

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Concurrent
import Control.Concurrent.STM

import Control.Lens
import Data.Maybe

import Control.Monad
import System.Random (randomRIO)


{-
    Choose which piece to try download next from a peer (rarest first algorithm)
    our piece choosing algorithm:
    if the peer has pieces that we are currently downloading:
        then select a random one of these
    if not, then choose a random piece out of, the rarest pieces
        that the peer has that we need
-}

bitfieldChange :: Bool -> Bitfield -> TVar RFInfo -> STM Bool
bitfieldChange isNew bf rfInfoT = do
    rfInfo <- readTVar rfInfoT
    let rarityMap' = Set.foldl (\rMap x -> Map.adjust (if isNew then (+1) else (+ (-1))) x rMap) (rarityMap rfInfo) bf
    writeTVar rfInfoT rfInfo{
        rarityMap = rarityMap'
    } 
    return $ not $ Set.null $ Set.intersection bf (remainingPieces rfInfo) 

{-
    given a peers bitfield and global rfInfo, choose a piece to download
    only allow two peers to jump onto the same piece, so remove piece from inProgress when the third joins
    if we cannot download from inprogress, start a new piece and add to inprogress
-}


choosePiece :: Int -> Bitfield -> TVar RFInfo -> STM (Maybe PieceIdx)
choosePiece rand bf rfInfoT = do
    rfInfo <- readTVar rfInfoT
    let intersection = Set.intersection bf (inProgress rfInfo)
    if not $ Set.null intersection then do -- there are pieces inProgress in our bitfield
        let 
            chosenIdx = Set.elemAt (rand `mod` Set.size intersection) (intersection)
            nDownloaders = fromJust $ Map.lookup chosenIdx (inProgressMp rfInfo) 
        writeTVar rfInfoT rfInfo {inProgressMp = Map.adjust (+ 1) chosenIdx (inProgressMp rfInfo), 
                                    -- if another peer is downloading, remove from inprogress so no one else downloads
                                  inProgress = if nDownloaders > 1 then Set.delete chosenIdx (inProgress rfInfo) 
                                                else inProgress rfInfo
                                 }
        return $ Just chosenIdx 
        else do -- choose the rarest piece that is not inProgress
            let remainingIntersection = Set.intersection bf (remainingPieces rfInfo)
            if Set.null remainingIntersection then return Nothing 
            else do
                let 
                    rarestIdxs = findRarests (rarityMap rfInfo) remainingIntersection
                    chosenIdx = Set.elemAt (rand `mod` Set.size rarestIdxs) rarestIdxs
                writeTVar rfInfoT rfInfo {
                                inProgressMp = Map.adjust (+ 1) chosenIdx (inProgressMp rfInfo),
                                inProgress = Set.insert chosenIdx (inProgress rfInfo)
                                }
                return $ Just chosenIdx


findRarests :: Map.Map PieceIdx Int -> Bitfield ->  Set.Set PieceIdx
findRarests rarityMap bf = snd $ Set.foldl isRarest (2^30, Set.empty) bf
    where 
        isRarest (r, st) x = do
            let rareness = fromJust $ Map.lookup x rarityMap

            if rareness > r then (r,st) 
            else if rareness == r then (r, Set.insert x st)
            else (rareness, Set.singleton x)




{-
    more complexity to global structures but quicker rarest piece selection:
-}

-- bitfieldChange :: Bool -> Bitfield -> Rarity -> Rarity
-- bitfieldChange isNew bf r = Set.foldl (alterRareness isNew) r bf
--     where
--         alterRareness :: Bool -> Rarity ->  PieceIdx -> Rarity
--         alterRareness isNew r pIdx = 
--             let 
--                 curRareness = case Map.lookup pIdx (rMap r) of 
--                     Just n -> n
--                     Nothing -> 0
--                 newRareness = if isNew then curRareness+1 else curRareness-1 in
--             Rarity {
--                 inProgress = inProgress r,
--                 rMap = 
--                     let rMap' = Map.delete pIdx (rMap r) in
--                         Map.insert pIdx newRareness rMap',
--                 rList = 
--                     let rList' = (rList r) & element curRareness .~ (Set.delete pIdx ((rList r) !! curRareness)) in
--                    rList' & element (newRareness) .~ (Set.insert pIdx ((rList r)!!newRareness))
--             }


-- choosePiece :: Bitfield -> Rarity -> IO (Maybe Int)
-- choosePiece bf r = do
--     let inProgress' = Set.intersection bf (inProgress r)
--     if not $ Set.null inProgress' then do-- has a piece that is in progress
--         Just <$> (randomElementList $ findRarest (rMap r) (2^30) (Set.toList inProgress') [])
--         else do-- doesnt have a piece thats in progress, find rarest needed piece
--             let rareIdxs = findRarest' (rList r) bf
--             -- print $ rList r
--             -- print $ rareIdxs
--             case rareIdxs of 
--                 Nothing -> do -- peer no longer useful to us
--                     return Nothing
--                 Just rareIdxs' -> Just <$> (randomElement rareIdxs')

-- findRarest' :: [Set.Set PieceIdx] -> Bitfield -> Maybe (Set.Set Int)
-- findRarest' [] bf = Nothing 
-- findRarest' (x:xs) bf = do
--     let inter = Set.intersection x bf 
--     if not $ Set.null inter then Just inter else findRarest' xs bf

-- -- findRarest :: (Ord a1, Ord a2) => Map.Map a1 a2 -> a2 -> [a1] -> [a1] -> [a1]
-- -- findRarest :: Map.Map Int Int -> Int -> []
-- findRarest mp curMin [] out = out
-- findRarest mp curMin (x:xs) out = case Map.lookup x mp of 
--     Just xCount -> if xCount < curMin then findRarest mp curMin xs [x] 
--     else if xCount == curMin then findRarest mp xCount xs (x:out) else
--         findRarest mp curMin xs out

-- -- when a new peer joins or when a peer disconnects, we must alter the piece rarity
-- bitfieldChange :: Bool -> Bitfield -> Rarity -> Rarity
-- bitfieldChange isNew bf r = Set.foldl (alterRareness isNew) r bf
--     where
--         alterRareness :: Bool -> Rarity ->  PieceIdx -> Rarity
--         alterRareness isNew r pIdx = 
--             let 
--                 curRareness = case Map.lookup pIdx (rMap r) of 
--                     Just n -> n
--                     Nothing -> 0
--                 newRareness = if isNew then curRareness+1 else curRareness-1 in
--             Rarity {
--                 inProgress = inProgress r,
--                 rMap = 
--                     let rMap' = Map.delete pIdx (rMap r) in
--                         Map.insert pIdx newRareness rMap',
--                 rList = 
--                     let rList' = (rList r) & element curRareness .~ (Set.delete pIdx ((rList r) !! curRareness)) in
--                    rList' & element (newRareness) .~ (Set.insert pIdx ((rList r)!!newRareness))
--             }
    
-- {- Given a piece idx, remove piece idx from pieceRarity by:
--     1. remove from inProgress
--     2. find rareness idx of piece from rMap, using this remove from rList
-- -}

-- -- when we initialize the client, create a blank rarity with rareness of all pieces == 0
-- mkBlankRarity :: Lengths -> Rarity
-- mkBlankRarity lns = 
--     Rarity {
--         inProgress = Set.empty,
--         rList = [Set.fromList [0..((nPieces lns)-1)]] ++ replicate 100 Set.empty, -- change later
--         rMap = Map.fromList $ zip [0..(nPieces lns)-1] (replicate (nPieces lns) 0)
--     }



-- -- remove piece from rarity by removing from inprogress, and removing it from rlist
-- -- don't need to remove from rMap since the peer is still connected
-- removeRarity :: PieceIdx -> Rarity -> Rarity
-- removeRarity pIdx r = do
--     let 
--         rarityIdx' = fromJust $ Map.lookup pIdx (rMap r)
--         raritySet = (rList r) !! rarityIdx'

--     Rarity {
--         inProgress = Set.delete pIdx (inProgress r),
--         rList = (rList r) & element rarityIdx' .~ (Set.delete pIdx raritySet),
--         rMap = rMap r
--     }

-- -- remove a piece from global
-- removeGlobal :: Global -> PieceIdx -> STM ()
-- removeGlobal g pIdx =  do
--     remaining <- readTVar (remainingPiecesT g) -- remove from remaining Pieces
--     writeTVar (remainingPiecesT g) (Set.delete pIdx remaining)

--     rarity <- readTVar (rarityT g) -- remove from rarity
--     writeTVar (rarityT g) (removeRarity pIdx rarity)

-- -- add a piece back into rarity by adding it into inProgress and adding it into rList
-- -- this is used when we have an async exception when downloading the last blocks of a piece
-- addRarity :: PieceIdx -> Rarity -> Rarity
-- addRarity pIdx r = do
--     let 
--         rarityIdx' = case Map.lookup pIdx (rMap r) of 
--             Just idx -> idx
--         raritySet = (rList r) !! rarityIdx'
--     Rarity {
--         inProgress = (Set.insert pIdx (inProgress r)), -- dont put back into inprogress, let the peer choose
--         rList = (rList r) & element rarityIdx' .~ (Set.insert pIdx raritySet),
--         rMap = rMap r
--     }

-- -- add a piece idx from remaining pieces, and rlist, inprogress (within rarity)
-- addGlobal :: Global -> PieceIdx -> STM ()
-- addGlobal g pIdx =  do
--     remaining <- readTVar (remainingPiecesT g) -- remove from remaining Pieces
--     writeTVar (remainingPiecesT g) (Set.insert pIdx remaining)

--     rarity <- readTVar (rarityT g) -- remove from rarity
--     writeTVar (rarityT g) (removeRarity pIdx rarity)
