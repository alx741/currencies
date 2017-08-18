module Data.Currency.Currencies
    ( -- * Currency Class
      Currency(..)

      -- * Monetary Amount
      , Amount(..)

      -- * Currencies
    , CLP(..)
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

-- | Chilean Peso
data CLP = CLP deriving (Show)

-- | Europian Union Euro
data EUR = EUR deriving (Show)

-- | US Dollar
data USD = USD deriving (Show)

instance Currency CLP where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "152"
    decimalDigits _ = 0
    symbol _ = Just "$"
    exchangeUSD _ = 0.0015
    countries _ = ["CL"]

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

instance Currency USD where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "840"
    decimalDigits _ = 2
    symbol _ = Just "$"
    exchangeUSD _ = 1.0
    countries _ = ["US", "AS", "BB", "BM", "IO", "VG", "BQ", "EC", "MH"
        , "FM", "MP", "PW", "PA", "PR", "TL", "TC", "VI"]
