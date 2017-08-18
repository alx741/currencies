{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Currency.Currencies
    ( IsCurrency(..)

      -- * Currencies
    , EUR(..)
    , USD(..)
    ) where

class IsCurrency a where
    currencyCode :: a -> String
    currencyNumericCode :: String
    decimalDigits :: Int
    convertionUSD :: Float

-- | US Dollar
data USD = USD deriving (Show)

-- | Europian Union Euro
data EUR = EUR deriving (Show)

instance IsCurrency EUR where
    currencyCode = show
    currencyNumericCode = ""
    decimalDigits = 2
    convertionUSD = 0.8

instance IsCurrency USD where
    currencyCode = show
    currencyNumericCode = ""
    decimalDigits = 2
    convertionUSD = 1.0
