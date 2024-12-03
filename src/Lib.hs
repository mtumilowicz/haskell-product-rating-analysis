{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( Purchase (..),
    loadPurchases,
    mostRatedProduct,
    bestAverageRatedProduct,
    MostRatedProduct (..),
    BestAverageRatedProduct (..),
  )
where

import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.HashMap.Strict as HM
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
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

mkPurchase :: String -> String -> String -> Int -> Either String Purchase
mkPurchase bId sId pId rtg =
  first show $
    Purchase
      <$> refine bId
      <*> refine sId
      <*> refine pId
      <*> refine rtg

instance FromNamedRecord Purchase where
  parseNamedRecord r = do
    bId <- r .: "Buyer Id"
    sId <- r .: "Shop Id"
    pId <- r .: "Product Id"
    rtg <- r .: "Rating"
    either fail pure $ mkPurchase bId sId pId rtg

loadPurchases :: FilePath -> IO [Either String Purchase]
loadPurchases filePath = do
  csvData <- BL.readFile filePath
  case decodeByName csvData of
    Left err -> return [Left err]
    Right (_, vec) ->
      return $ map (runParser . parseNamedRecord) (V.toList vec)

mostRatedProduct :: [Purchase] -> Maybe MostRatedProduct
mostRatedProduct [] = Nothing
mostRatedProduct purchases = Just $ mkMostRatedProduct $ maximumBy (comparing snd) (HM.toList ratingsMap)
  where
    ratingsMap = HM.fromListWith (+) [(productId p, 1 :: Int) | p <- purchases]
    mkMostRatedProduct (pid, count) =
      MostRatedProduct
        { productName = unrefine pid,
          ratedCount = count
        }

average :: [Int] -> Double
average [] = 0
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

bestAverageRatedProduct :: [Purchase] -> Maybe BestAverageRatedProduct
bestAverageRatedProduct [] = Nothing
bestAverageRatedProduct purchases =
  Just $ mkBestAverageRatedProduct $ maximumBy (comparing snd) (HM.toList averages)
  where
    ratingsMap = HM.fromListWith (++) [(productId p, [unrefine $ rating p]) | p <- purchases]
    averages = HM.map average ratingsMap
    mkBestAverageRatedProduct (pid, avg) =
      BestAverageRatedProduct
        { productName = unrefine pid,
          averageRating = avg
        }
