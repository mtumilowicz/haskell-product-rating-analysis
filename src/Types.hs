{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Purchase (..),
    MostRatedProduct (..),
    BestAverageRatedProduct (..),
  )
where

import Data.Bifunctor (first)
import Data.Csv
import Refined
import Refined.MatchesRegex

type BuyerId = Refined (MatchesRegex "^[a-zA-Z][a-zA-Z0-9_.-]*$") String

type ShopId = Refined (MatchesRegex "^[a-zA-Z][a-zA-Z0-9_.-]*$") String

type ProductId = Refined (MatchesRegex "^[a-zA-Z][a-zA-Z0-9\\-]+-[0-9]{2}$") String

type Rating = Refined (FromTo 1 5) Int

data Purchase = Purchase
  { buyerId :: BuyerId,
    shopId :: ShopId,
    productId :: ProductId,
    rating :: Rating
  }
  deriving (Show)

instance FromNamedRecord Purchase where
  parseNamedRecord r = do
    bId <- r .: "Buyer Id"
    sId <- r .: "Shop Id"
    pId <- r .: "Product Id"
    rtg <- r .: "Rating"
    either fail pure $ mkPurchase bId sId pId rtg

data MostRatedProduct = MostRatedProduct
  { productName :: String,
    ratedCount :: Int
  }
  deriving (Show, Eq)

data BestAverageRatedProduct = BestAverageRatedProduct
  { productName :: String,
    averageRating :: Double
  }
  deriving (Show, Eq)

mkPurchase :: String -> String -> String -> Int -> Either String Purchase
mkPurchase bId sId pId rtg =
  first show $
    Purchase
      <$> refine bId
      <*> refine sId
      <*> refine pId
      <*> refine rtg
