{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Either (partitionEithers)
import Lib

main :: IO ()
main = do
  results <- loadPurchases "resources/file.csv"
  let (_, valid) = partitionEithers results
  case mostRatedProduct valid of
    Nothing -> putStrLn "No ratings available."
    Just entry -> putStrLn $ "Most Rated Product: " ++ show entry
  case bestAverageRatedProduct valid of
    Nothing -> putStrLn "No average ratings available."
    Just entry -> putStrLn $ "Best Average Rated Product: " ++ show entry

--   mapM_ handleResult results
-- where
--   handleResult :: Either String Purchase -> IO ()
--   handleResult (Left err) = putStrLn $ "Error: " ++ err
--   handleResult (Right purchase) = print purchase
