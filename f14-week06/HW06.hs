{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- Converts "Y" to True, "N" to False
ynToBool :: Value -> Value
ynToBool (Data.Aeson.String "Y") = Bool True
ynToBool (Data.Aeson.String "N") = Bool False
ynToBool (Object o) = Object (fmap ynToBool o)
ynToBool (Array o) = Array (fmap ynToBool o)
ynToBool o = o

parseData :: B.ByteString -> Either String Value
parseData s = fmap ynToBool (eitherDecode s)
-- Above impl of parseData uses Either's Functor instance, making it equivalent
-- to:
--
-- parseData s = case eitherDecode s of
--   Left ss -> Left ss
--   Right v -> Right $ ynToBool v


-- Testing ynToBool
-- test1 :: B.ByteString
-- test1 = ("{\"a\": \"Y\"}" :: B.ByteString)
-- testY :: Either String Value
-- testY = parseData ("{\"a\": \"Y\"}" :: B.ByteString)
-- testN :: Either String Value
-- testN = parseData ("{\"a\": \"N\"}" :: B.ByteString)
-- yy :: T.Text
-- yy = "Y"
-- nn :: T.Text
-- nn = "N"

data Market = Market { marketname :: T.Text,
                       x :: Float,
                       y :: Float,
                       city :: T.Text,
                       state :: T.Text,
                       credit :: Bool,
                       bakedgoods :: Bool }
  deriving (Generic, Show, Eq)

-- Automatically derive a FromJSON instance for Market. This is why we needed to
-- derive Generic. So we can do:
--
-- p :: Either String Market
-- p = eitherDecode "{marketname: \"My Farmer's Market\"}"
-- p == Right (Market { marketname = "My Farmer's Market" })
instance FromJSON Market


parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets bs = case parseData bs of
  Left s -> Left s
  Right v -> case fromJSON v of
    Error s -> Left s
    Success markets -> Right markets

loadData :: IO [Market]
loadData = do
  marketStrings <- B.readFile "markets.json"
  case (parseMarkets marketStrings) of
    Left err -> fail err
    Right markets -> return markets

-- In GHCi:
--
-- mkts <- loadData

data OrdList a = OrdList { getOrdList :: [a] }
  deriving (Eq, Show)

-- Zips two lists together in ordered style (e.g. merge part of merge-sort).
-- Assumes given lists are ordered.
--
-- Example:
--
-- zipOrdered [2, 4, 6, 10] [1, 3, 5, 7, 9]
-- [1,2,3,4,5,6,7,9,10]
--
zipOrdered :: Ord a => [a] -> [a] -> [a]
zipOrdered [] [] = []
zipOrdered (u:us) [] = u : zipOrdered us []
zipOrdered [] (v:vs) = v : zipOrdered [] vs
zipOrdered (u:us) (v:vs)
  | u <= v    = u : zipOrdered us (v:vs)
  | otherwise = v : zipOrdered (u:us) vs

-- We're making a Monoid instance for OrdList a, where a is a type that has an
-- Ord instance (is orderable). So, an ordered list of orderable things.
instance Ord a => Monoid (OrdList a) where
  -- mempty :: m
  mempty = OrdList { getOrdList = [] }

  -- mappend :: m -> m -> m
  mappend m1 m2 = OrdList { getOrdList = zipOrdered (getOrdList m1) (getOrdList m2) }

  -- mconcat :: [a] -> a, fold a list using monoid
  -- Use default implementation.

type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search f t mkts = mconcat (foldl (\ms m -> if (T.isInfixOf t (marketname m))
                                           then (f m):ms
                                           else ms)
                           [] mkts)

-- Examples:
--
-- Sum count of markets (Note that Sum 1 tells us to use the Sum monoid for
-- ints):
--
-- search (\_ -> (Sum 1)) T.empty mkts
-- search (\_ -> (Sum 1)) (T.pack "Farmer")  mkts

marketWithName :: Market -> (T.Text, Market)
marketWithName mkt@(Market { marketname = name }) = (name, mkt)

