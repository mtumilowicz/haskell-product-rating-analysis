{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Lib
import Refined
import Test.Hspec
import Types

main :: IO ()
main = hspec $ do
  describe "mostRatedProduct" $ do
    it "returns the product with the highest number of ratings" $ do
      -- given
      let purchases = testPurchases

      -- and
      let expectedProductId = "productB-02"

      -- when
      let mostRated = mostRatedProduct purchases

      -- then
      mostRated `shouldBe` Just (MostRatedProduct expectedProductId 3)

  describe "bestAverageRatedProduct" $ do
    it "returns the product with the highest average rating" $ do
      -- given
      let purchases = testPurchases

      -- and
      let expectedProduct = "productC-03"
      let expectedAverage = 5

      -- when
      let bestAverageRated = bestAverageRatedProduct purchases

      -- then
      bestAverageRated `shouldBe` Just (BestAverageRatedProduct expectedProduct expectedAverage)

testPurchases :: [Purchase]
testPurchases =
  [ Purchase
      { buyerId = $$(refineTH "buyer1"),
        shopId = $$(refineTH "shop1"),
        productId = $$(refineTH "productA-01"),
        rating = $$(refineTH 5)
      },
    Purchase
      { buyerId = $$(refineTH "buyer2"),
        shopId = $$(refineTH "shop1"),
        productId = $$(refineTH "productA-01"),
        rating = $$(refineTH 4)
      },
    Purchase
      { buyerId = $$(refineTH "buyer3"),
        shopId = $$(refineTH "shop2"),
        productId = $$(refineTH "productB-02"),
        rating = $$(refineTH 5)
      },
    Purchase
      { buyerId = $$(refineTH "buyer4"),
        shopId = $$(refineTH "shop2"),
        productId = $$(refineTH "productB-02"),
        rating = $$(refineTH 5)
      },
    Purchase
      { buyerId = $$(refineTH "buyer5"),
        shopId = $$(refineTH "shop2"),
        productId = $$(refineTH "productB-02"),
        rating = $$(refineTH 4)
      },
    Purchase
      { buyerId = $$(refineTH "buyer6"),
        shopId = $$(refineTH "shop3"),
        productId = $$(refineTH "productC-02"),
        rating = $$(refineTH 3)
      },
    Purchase
      { buyerId = $$(refineTH "buyer7"),
        shopId = $$(refineTH "shop3"),
        productId = $$(refineTH "productC-03"),
        rating = $$(refineTH 5)
      },
    Purchase
      { buyerId = $$(refineTH "buyer8"),
        shopId = $$(refineTH "shop3"),
        productId = $$(refineTH "productC-03"),
        rating = $$(refineTH 5)
      }
  ]
