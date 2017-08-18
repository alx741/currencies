module Data.Currency.Currencies
    ( -- * Currency Class
      Currency(..)

      -- * Monetary Amount
      , Amount(..)

      -- * Currencies
    , EUR(..)
    , USD(..)
    ) where

-- | Monetary amounts
data Amount a = Amount a Double deriving (Show, Read, Eq, Ord) -- FIXME: Ord manual instance

data CurrencyType = Circulating | Local | Fictional deriving (Show, Read, Eq)

class (Show c) => Currency c where
    currencyType :: c -> CurrencyType
    -- | ISO 4217 Currency Code
    isoCode :: c -> String
    -- | ISO 4217 Currency Numeric Code
    isoNumericCode :: c -> String
    -- | Number of digits after the decimal separator
    decimalDigits :: c -> Int
    -- | Currencty UTF8 symbol
    symbol :: c -> Maybe String

    exchangeUSD :: c -> Float
    -- | ISO 3166-1 alpha-2 Country codes where the currency is used
    countries :: c -> [String]

-- | US Dollar
data USD = USD deriving (Show)

-- | Europian Union Euro
data EUR = EUR deriving (Show)

instance Currency USD where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "840"
    decimalDigits _ = 2
    symbol _ = Just "$"
    exchangeUSD _ = 1.0
    countries _ = ["US", "AS", "BB", "BM", "IO", "VG", "BQ", "EC", "MH"
        , "FM", "MP", "PW", "PA", "PR", "TL", "TC", "VI"]

instance Currency EUR where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "978"
    decimalDigits _ = 2
    symbol _ = Just "€"
    exchangeUSD _ = 1.17
    countries _ = ["AD", "AT", "BE", "CY", "EE", "FI", "FR", "DE", "GR"
        , "GP", "IE", "IT", "LV", "LT", "LU", "MT", "MQ", "YT", "MC"
        , "ME", "NL", "PT", "RE", "BL", "PM", "SM", "SK", "SI", "ES"]
