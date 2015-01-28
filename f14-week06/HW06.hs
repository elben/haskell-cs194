{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L

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

  -- mconcat :: [OrdList a] -> OrdList a, fold a list using monoid
  -- Use default implementation.
  -- A better implementation would implement merge sort here.

-----------------
-- Exercise 6
-----------------

type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search f t mkts = (foldl (\ms m -> if (T.isInfixOf t (marketname m))
                                           then (f m) <> ms -- <> is the Monoid mconcat
                                           else ms)
                           mempty mkts)

-- Examples:
--
-- Sum count of markets (Note that Sum 1 tells us to use the Sum monoid for
-- ints):
--
-- search (\_ -> (Sum 1)) T.empty mkts
-- search (\_ -> (Sum 1)) (T.pack "Farmer") mkts

marketWithName :: Market -> (T.Text, Market)
marketWithName mkt@(Market { marketname = name }) = (name, mkt)

-----------------
-- Exercise 7
-----------------

-- Returns the first market found by a search, if any are found at all.
firstFound :: Searcher (Maybe Market)
firstFound t mkts = L.find (\m -> T.isInfixOf t (marketname m)) mkts

-- Examples:
--
-- mkts <- loadData
--
-- firstFound (T.pack "Farmer") mkts
-- firstFound (T.pack "Church") mkts
-- firstFound (T.pack "Fruit") mkts
-- firstFound (T.pack "Waterfall") mkts
-- firstFound (T.pack "Garden") mkts

-----------------
-- Exercise 8
-----------------

lastFound :: Searcher (Maybe Market)
lastFound t mkts = L.find (\m -> T.isInfixOf t (marketname m)) (reverse mkts)

-- mkts <- loadData
--
-- lastFound (T.pack "Farmer") mkts
-- lastFound (T.pack "Church") mkts

-----------------
-- Exercise 9
-----------------

-- Uses the Monoid for lists, basically:
--
-- [m] <> mempty :: List m
--
-- which is, the same as:
-- [m] ++ []
--
allFound :: Searcher [Market]
allFound t mkts = search (\m -> [m]) t mkts

-- mkts <- loadData
--
-- allFound (T.pack "Farmer") mkts
-- allFound (T.pack "Church") mkts

-----------------
-- Exercise 10
-----------------

numberFound :: Searcher Integer
numberFound t mkts = getSum $ search (\_ -> Sum 1) t mkts

-----------------
-- Exercise 11
-----------------
--
-- Returns all the markets found by a search, ordered from northernmost to
-- southernmost. You will need a wrapper around Market to choose an appropriate
-- Ord instance. This exercise doesn’t take too much code, but getting the
-- structure right is intended to be a challenge.
--
-- You may find that your function takes a little while to run. As an optional
-- extra, make it work more efficiently by adding a definition for mconcat to
-- the Monoid instance for OrdList and make sure your search function uses
-- mconcat. The default definition for mconcat puts elements together one by
-- one, but you can write a custom one that maintains the ordering in a more
-- efficient fashion.


-- We create a wrapper data type so that we don't have to specify a north-biased
-- ordering for ALL markets. That is, there are many ways of ordering a Market.
-- So, if we *want* a North-biased ordering, we wrap it in NorthBiased. This is
-- simliar to how Data.Monoid provides Product and Sum wrappers.
data NorthBiased = NorthBiased { getMarket :: Market }
  deriving (Eq, Show)

-- Create an Ord instance with the wrapper data type, so that we know how to
-- order the north-biased markets (the norther the better)
instance Ord NorthBiased where
  (<=) a b = (y (getMarket b)) <= (y (getMarket a))

-- instance Ord Market where
--   (<=) a b = (y a) <= (y b)

-- We use OrdList, which has a Monoid instance. In the Monoid instance, OrdList
-- is combined (<*> and mconcat) in an ordered way using the zip-merge above, as
-- implemented. This, in essence, is insertion sort.
--
-- An improvement would be to implement mconcat for OrdList's monoid
-- implementation as a faster sort, like merge sort.

orderNtoS :: Searcher [Market]
orderNtoS t mkts = map getMarket (getOrdList $ search (\m -> OrdList [NorthBiased m]) t mkts)

data X = X { getX :: String}
  deriving (Eq, Show)

instance Ord X where
  (<=) a b = b >= a

-- Example:
--
-- mkts <- loadData
-- map state $ orderNtoS (T.pack "Sun") mkts
