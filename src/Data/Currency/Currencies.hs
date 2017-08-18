{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Currency.Currencies
    ( -- * Currency Class
      IsCurrency(..)

      -- * Monetary Amount
      , Amount
      , amount

      -- * Currencies
    , EUR(..)
    , USD(..)
    ) where

-- | Monetary amounts
data Amount a = Amount a Double deriving (Show, Read, Eq, Ord) -- FIXME: Ord manual instance

-- | Create a monetary amount of a particular currency
amount :: (IsCurrency c) => c -> Double -> Amount c
amount = Amount

data CurrencyType = Circulating | Local | Fictional deriving (Show, Read, Eq)

class IsCurrency a where
    currencyType :: CurrencyType
    -- | ISO 4217 Currency Code
    isoCode :: a -> String
    -- | ISO 4217 Currency Numeric Code
    isoNumericCode :: String
    -- | Number of digits after the decimal separator
    decimalDigits :: Int
    -- | Currencty UTF8 symbol
    symbol :: Maybe String

    exchangeUSD :: Float
    -- | ISO 3166-1 alpha-2 Country codes where the currency is used
    countries :: [String]

-- | US Dollar
data USD = USD deriving (Show)

-- | Europian Union Euro
data EUR = EUR deriving (Show)

instance IsCurrency USD where
    currencyType = Circulating
    isoCode = show
    isoNumericCode = "840"
    decimalDigits = 2
    symbol = "$"
    exchangeUSD = 1.0
    countries = ["US", "AS", "BB", "BM", "IO", "VG", "BQ", "EC", "MH"
        , "FM", "MP", "PW", "PA", "PR", "TL", "TC", "VI"]

instance IsCurrency EUR where
    currencyType = Circulating
    isoCode = show
    isoNumericCode = "978"
    decimalDigits = 2
    symbol = "€"
    exchangeUSD = 1.17
    countries = ["AD", "AT", "BE", "CY", "EE", "FI", "FR", "DE", "GR"
        , "GP", "IE", "IT", "LV", "LT", "LU", "MT", "MQ", "YT", "MC"
        , "ME", "NL", "PT", "RE", "BL", "PM", "SM", "SK", "SI", "ES"]
