{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Bencode where 

import qualified Data.ByteString as B 
import qualified Data.Map as Map 
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as C 

import qualified Control.Lens as L
import Control.Applicative

import Types

{-
    Bencode parser, and lenses for accessing deeply nested bencodes
-}


instance Show Ben where show = showBen
showBen :: Ben -> String 
showBen (String s) = show s
showBen (Integer i) = show i
showBen (List l) = "[" ++ foldl f "" l ++ "]" where 
  f a x = showBen x ++ ", " ++ a
showBen (Dict d) = "{" ++ Map.foldrWithKey f "" d ++ "}" where 
  f k v a = showBen k ++ " : " ++ showBen v ++ ", " ++ a

parseString :: P.Parser Ben
parseString = do
  ln <- P.decimal 
  P.char ':'
  str <-  P.take ln
  return $ String str

parseInteger :: P.Parser Ben
parseInteger  = (Integer . read) <$> (P.char 'i' >>  P.manyTill P.anyChar (P.char 'e'))

parseList :: P.Parser Ben
parseList = List <$> (P.char 'l' >> P.manyTill parseBen (P.char 'e'))

parseDict :: P.Parser Ben
parseDict = do
  P.char 'd'
  elems <- P.manyTill parsePair (P.char 'e')
  return $ Dict $ Map.fromList elems 
    where
      parsePair = do
        key <- parseString
        value <- parseBen
        return $ (key, value)  
  
parseBen :: P.Parser Ben
parseBen = parseString <|> parseInteger <|> parseList <|> parseDict

readExpr :: B.ByteString -> Ben
readExpr bs = case P.parseOnly parseBen bs of 
  Right val -> val

-- ***************** Lenses

bstring :: L.Traversal' Ben B.ByteString
bstring f (String s) = String <$> f s
bstring _ bv = pure bv

bnumber :: L.Traversal' Ben Int
bnumber f (Integer n) = Integer <$> f n
bnumber _ bv = pure bv

blist :: L.Traversal' Ben Ben
blist f (List xs) = do
  let a = traverse f xs
  let y = List <$> a
  y
blist _ bv = pure bv

-- bll :: L.Traversal' Ben Ben
-- bll f (List xs) = do
--   let y = traverse f xs
--   List <$> y

bkey :: String -> L.Traversal' Ben Ben
bkey k f bv@(Dict m) = case Map.lookup (String (C.pack k)) m of
                               Just v -> f v
                               Nothing -> pure bv
bkey _ _ bv = pure bv

bsl :: L.Lens' Ben Int
bsl = L.lens (\ (Integer b) -> b) (\bn newS -> Integer newS)


-- ******************** Serialise

serialize :: Ben -> B.ByteString
serialize (String s) = B.concat $ [(C.pack $ (show (B.length s)) <> ":"), s]
serialize (Integer i) = C.pack $ "i" <> show i <> "e"
serialize (List xs) = B.concat $ [C.pack "l"] ++  map serialize xs ++ [C.pack "e"]
serialize (Dict d) = B.concat $ [C.pack "d"] ++ (Map.elems (Map.mapWithKey f d)) 
  ++ [C.pack "e"] where 
  f key x = B.concat [serialize key, serialize x]

