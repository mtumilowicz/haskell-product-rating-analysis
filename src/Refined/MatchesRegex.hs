{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Refined.MatchesRegex
  ( MatchesRegex,
  )
where

import Data.Proxy (Proxy (..))
import Data.Typeable (typeOf)
import GHC.TypeLits
  ( KnownSymbol,
    Symbol,
    symbolVal,
  )
import Refined
import Text.Regex.TDFA ((=~))

data MatchesRegex (regex :: Symbol)

instance (KnownSymbol regex) => Predicate (MatchesRegex regex) String where
  validate p x = do
    let pattern = symbolVal (Proxy @regex)
    if x =~ pattern
      then Nothing
      else
        throwRefineOtherException
          (typeOf p)
          "Input does not match the regex pattern: "
