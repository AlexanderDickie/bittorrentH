module Utils where

import qualified Data.ByteString as B (ByteString, pack, unpack)
import qualified Data.Set as Set

import Data.Word (Word8)
import Text.Printf (printf)
import System.Random (randomRIO)

int2bs :: Int -> B.ByteString 
int2bs i = B.pack $ [fromIntegral i :: Word8]  

bs2int :: B.ByteString -> Int 
bs2int bs = fromIntegral ((B.unpack bs) !! 0)

w82int :: Word8 -> Int
w82int w = fromIntegral w :: Int

toHex :: B.ByteString -> String
toHex bytes = B.unpack bytes >>= (printf "%02x") 

toUrl :: B.ByteString -> String
toUrl bytes = foldl (\b a -> b  ++ "%" ++ printf "%02x" a) "" $ (B.unpack) bytes

unWrap :: Maybe a -> a
unWrap (Just x) = x

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

sliceBs :: Int -> Int -> B.ByteString -> B.ByteString 
sliceBs from to bs = B.pack $ slice from to (B.unpack bs)

-- *********
dec2seq :: Int -> Int -> Int -> Maybe [Int]
dec2seq _ 1 _ = Nothing
dec2seq targetLn factor dec = expand targetLn $ reverse $ dec2seq' dec
  where
    dec2seq' 0 = []
    dec2seq' x = (x `mod` factor) : dec2seq' (x `quot` factor)

expand :: Int -> [Int] -> Maybe [Int]
expand targetLn xs = let difference = targetLn - length xs in
  if difference == 0 then Just xs else
    if difference > 0 then Just $ (replicate difference 0) ++ xs else Nothing
-- -- *********

dec2endian' :: Int -> Maybe B.ByteString 
dec2endian' dec = pack <$> (dec2seq 4 256 dec) where
  pack x = B.pack $ map (\x -> fromIntegral x :: Word8) x

unJust :: Maybe a -> a
unJust (Just x) = x

set2bf :: Int -> [Int] -> [Int]
set2bf _ [] = []
set2bf i (x:xs) = if x == 0 then set2bf (i+1) xs else [i] ++ (set2bf (i+1) xs) 

dec2endian :: Int -> B.ByteString
dec2endian dec = unJust $ dec2endian' dec

byte2bits' :: Word8 -> Maybe [Int]
byte2bits' dec = dec2seq 8 2 (fromIntegral dec :: Int)

byte2bits :: Word8 -> [Int]
byte2bits dec = unJust $ byte2bits' dec

endian2int :: B.ByteString -> Int
endian2int bs = toDec $ reverse w8
  where
    w8 = map (\x -> fromIntegral x :: Int) $ B.unpack bs 
    toDec (x:[]) = x
    toDec (x:xs) = x + 256 * toDec xs

endian82int w8 = endian2int $ B.pack w8
        
-- ***************** 
randomElement :: Set.Set a -> IO a
randomElement set = do
   i <- randomRIO (0, Set.size set - 1)
   return $ Set.elemAt i set

randomElementList :: [a] -> IO a
randomElementList xs = do
  i <- randomRIO (0, length xs - 1)
  return $ xs !! i

fst' :: (a, b, c) -> a
fst' (a,b,c) = a
snd' :: (a, b, c) -> b
snd' (a,b,c) = b
trd' :: (a, b, c) -> c
trd' (a,b,c) = c

