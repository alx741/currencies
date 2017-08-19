module Data.Currency.Amounts
    ( -- * Monetary Amount
      Amount(..)
    ) where

import Data.Currency.Currencies

-- | Monetary amounts
data Amount c = Amount c Double deriving (Show, Read, Eq)

instance (Currency c) => Ord (Amount c) where
    (Amount _ v1) <= (Amount _ v2) = v1 <= v2
