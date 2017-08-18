module Data.Currency.Currencies
    ( -- * Currency Class
      Currency(..)

    -- * Monetary Amount
    , Amount(..)

      -- * Currencies
    , BTC(..)
    , CLP(..)
    , EUR(..)
    , USD(..)
    ) where

-- | Monetary amounts
data Amount a = Amount a Double deriving (Show, Read, Eq, Ord) -- FIXME: Ord manual instance

data CurrencyType
  = Circulating -- ^ Currencies recognized as legal tender
  | Local -- ^ Currencies with validity only in particular geographical localities
  | Supranational -- ^ Currencies for procedural purposes and precious metals (X currencies)
  | Cryptocurrency -- ^ Digital, cryptography based currencies
  | Fictional -- ^ Currencies used in games, movies, novels, and other fictional setups
  deriving (Show, Read, Eq)

class (Show c) => Currency c where
    currencyType :: c -> CurrencyType
    -- | ISO 4217 Currency Code
    isoCode :: c -> String
    -- | ISO 4217 Currency Numeric Code
    isoNumericCode :: c -> String
    -- | Number of digits after the decimal separator
    decimalDigits :: c -> Int
    -- | Currency UTF-8 symbol
    symbol :: c -> String

    exchangeUSD :: c -> Double
    -- | ISO 3166-1 alpha-2 Country codes where the currency is used
    countries :: c -> [String]

-- | Bitcoin
data BTC = BTC deriving (Show)

-- | Chilean Peso
data CLP = CLP deriving (Show)

-- | European Union Euro
data EUR = EUR deriving (Show)

-- | US Dollar
data USD = USD deriving (Show)


instance Currency BTC where
    currencyType _ = Cryptocurrency
    isoCode = show
    isoNumericCode _ = ""
    decimalDigits _ = 8
    symbol _ = "B"
    exchangeUSD _ = 4237.88
    countries _ = []

instance Currency CLP where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "152"
    decimalDigits _ = 0
    symbol _ = "$"
    exchangeUSD _ = 0.0015
    countries _ = ["CL"]

instance Currency EUR where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "978"
    decimalDigits _ = 2
    symbol _ = "€"
    exchangeUSD _ = 1.17
    countries _ = ["AD", "AT", "BE", "CY", "EE", "FI", "FR", "DE", "GR"
        , "GP", "IE", "IT", "LV", "LT", "LU", "MT", "MQ", "YT", "MC"
        , "ME", "NL", "PT", "RE", "BL", "PM", "SM", "SK", "SI", "ES"]

instance Currency USD where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "840"
    decimalDigits _ = 2
    symbol _ = "$"
    exchangeUSD _ = 1.0
    countries _ = ["US", "AS", "BB", "BM", "IO", "VG", "BQ", "EC", "MH"
        , "FM", "MP", "PW", "PA", "PR", "TL", "TC", "VI"]
