module Data.Currency.Amounts
    where
    -- ( -- * Monetary Amount
    --   Amount(..)
    -- ) where

import Data.Currency.Currencies

-- | Monetary amounts
data Amount c = Amount c Double deriving (Show, Read, Eq)

instance (Currency c) => Ord (Amount c) where
    (Amount _ v1) <= (Amount _ v2) = v1 <= v2

-- | Convert an 'Amount' to 'USD'
toUSD :: (Currency c) => Amount c -> Amount USD
toUSD (Amount c amount) = Amount USD $ amount * exchangeUSD c

-- | Convert an 'Amount' from 'USD' to a given 'Currency'
fromUSD :: (Currency c) => c -> Amount USD -> Amount c
fromUSD c (Amount USD amount) = Amount c $ amount / exchangeUSD c

-- | Convert an 'Amount' to a given 'Currency'
convert :: (Currency c', Currency c) => c' -> Amount c -> Amount c'
convert c' amount = fromUSD c' $ toUSD amount

-- | Compare two 'Amount' values of different currencies
compareAmounts :: (Currency c1, Currency c2) => Amount c1 -> Amount c2 -> Ordering
compareAmounts = undefined
