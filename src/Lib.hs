{-# LANGUAGE DuplicateRecordFields #-}

module Lib
  ( mostRatedProduct,
    bestAverageRatedProduct,
  )
where

import qualified Data.HashMap.Strict as HM
import Data.List (maximumBy)
import Data.Ord (comparing)
import Refined
import Types

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
