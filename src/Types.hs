{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Purchase (..),
    BuyerId,
    ShopId,
    ProductId,
    Rating,
    MostRatedProduct (..),
    BestAverageRatedProduct (..),
  )
where

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
