{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import Data.HashMap.Strict

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

iterateHashMap :: HashMap T.Text Value -> HashMap T.Text Value
iterateHashMap hm = fmap ynToBool hm

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


getResult :: Result a -> a
getResult (Success a) = a

parseMarkets :: B.ByteString -> Either String [Market]
-- parseMarkets bs = fmap getResult (fmap fromJSON (parseData bs))
-- Above uses Functors, which makes it equivalent to:
--
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
-- markets <- loadData
