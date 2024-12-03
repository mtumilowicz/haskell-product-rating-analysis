{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module In.CsvParser
  ( loadPurchases,
  )
where

import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Refined
import Types

data PurchaseRow = PurchaseRow
  { buyerId :: String,
    shopId :: String,
    productId :: String,
    rating :: Int
  }
  deriving (Show)

instance FromNamedRecord PurchaseRow where
  parseNamedRecord r = do
    bId <- r .: "Buyer Id"
    sId <- r .: "Shop Id"
    pId <- r .: "Product Id"
    rtg <- r .: "Rating"
    return $ PurchaseRow bId sId pId rtg

toDomainPurchase :: PurchaseRow -> Either String Purchase
toDomainPurchase (PurchaseRow bId sId pId rtg) =
  first show $
    Purchase
      <$> refine bId
      <*> refine sId
      <*> refine pId
      <*> refine rtg

loadPurchases :: FilePath -> IO [Either String Purchase]
loadPurchases filePath = do
  csvData <- BL.readFile filePath
  case decodeByName csvData of
    Left err -> return [Left err]
    Right (_, vec) ->
      return $ processRecord <$> (V.toList vec)
  where
    processRecord :: NamedRecord -> Either String Purchase
    processRecord purchaseNamedRecord =
      case runParser (parseNamedRecord purchaseNamedRecord) of
        Left err -> Left (show err)
        Right purchaseRow -> toDomainPurchase purchaseRow
