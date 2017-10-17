-- {-# LANGUAGE EmptyDataDecls #-}

-- | ISO 4217 compliant and other currencies

module Data.Currency.Currencies
    ( -- * Currency Class
      Currency(..)
    , CurrencyType(..)

      -- * Currencies
    , BRL(..)
    , BTC(..)
    , CAD(..)
    , CLP(..)
    , EUR(..)
    , JPY(..)
    , KPW(..)
    , KRW(..)
    , MXN(..)
    , PAB(..)
    , PEN(..)
    , PYG(..)
    , SIM(..)
    , USD(..)
    ) where

class (Show c, Eq c) => Currency c where
    currencyType :: c -> CurrencyType
    -- | ISO 4217 Currency Code
    isoCode :: c -> String
    -- | ISO 4217 Currency Numeric Code
    isoNumericCode :: c -> String
    -- | Number of digits after the decimal separator
    decimalDigits :: c -> Int
    -- | Currency UTF-8 symbol
    symbol :: c -> String
    -- | Exchange rate with US Dollar (USD)
    exchangeUSD :: c -> Double
    -- | ISO 3166-1 alpha-2 Country codes where the currency is used
    countries :: c -> [String]

data CurrencyType
  = Circulating -- ^ Currencies recognized as legal tender
  | Local -- ^ Currencies with validity only in particular geographical localities
  | Supranational -- ^ Currencies for procedural purposes and precious metals (X currencies)
  | Cryptocurrency -- ^ Digital, cryptography based currencies
  | Fictional -- ^ Currencies used in games, movies, novels, and other fictional setups
  deriving (Show, Read, Eq)


-- TODO: Strive to use 'Deriving Empty' with -XEmptyDataDecls in the future if it gets into GHC
-- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0006-deriving-empty.rst

-- | Brazilian Real
data BRL = BRL deriving (Show, Read, Eq)

-- | Bitcoin
data BTC = BTC deriving (Show, Read, Eq)

-- | Canadian Dollar
data CAD = CAD deriving (Show, Read, Eq)

-- | Chilean Peso
data CLP = CLP deriving (Show, Read, Eq)

-- | European Union Euro
data EUR = EUR deriving (Show, Read, Eq)

-- | Japanse Yen
data JPY = JPY deriving (Show, Read, Eq)

-- | North Korean Won
data KPW = KPW deriving (Show, Read, Eq)

-- | North Korean Won
data KRW = KRW deriving (Show, Read, Eq)

-- | Mexican Peso
data MXN = MXN deriving (Show, Read, Eq)

-- | Panamanian Balboa
data PAB = PAB deriving (Show, Read, Eq)

-- | Peruvian Sol
data PEN = PEN deriving (Show, Read, Eq)

-- | Paraguayan guaraní
data PYG = PYG deriving (Show, Read, Eq)

-- | SIM's Simoleon
data SIM = SIM deriving (Show, Read, Eq)

-- | US Dollar
data USD = USD deriving (Show, Read, Eq)


instance Currency BRL where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "986"
    decimalDigits _ = 2
    symbol _ = "$"
    exchangeUSD _ = 0.32
    countries _ = ["BR"]

instance Currency BTC where
    currencyType _ = Cryptocurrency
    isoCode = show
    isoNumericCode _ = ""
    decimalDigits _ = 8
    symbol _ = "B"
    exchangeUSD _ = 4237.88
    countries _ = []

instance Currency CAD where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "124"
    decimalDigits _ = 2
    symbol _ = "$"
    exchangeUSD _ = 0.79
    countries _ = ["CA"]

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
    exchangeUSD _ = 1.18
    countries _ = ["AD", "AT", "BE", "CY", "EE", "FI", "FR", "DE", "GR"
        , "GP", "IE", "IT", "LV", "LT", "LU", "MT", "MQ", "YT", "MC"
        , "ME", "NL", "PT", "RE", "BL", "PM", "SM", "SK", "SI", "ES"]

instance Currency JPY where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "392"
    decimalDigits _ = 0
    symbol _ = "¥"
    exchangeUSD _ = 0.0092
    countries _ = ["JP"]

instance Currency KPW where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "408"
    decimalDigits _ = 2
    symbol _ = "₩"
    exchangeUSD _ = 0.0077
    countries _ = ["KP"]

instance Currency KRW where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "410"
    decimalDigits _ = 0
    symbol _ = "₩"
    exchangeUSD _ = 0.000877
    countries _ = ["KR"]

instance Currency MXN where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "484"
    decimalDigits _ = 2
    symbol _ = "$"
    exchangeUSD _ = 0.06
    countries _ = ["MX"]

instance Currency PAB where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "590"
    decimalDigits _ = 2
    symbol _ = "B"
    exchangeUSD _ = 1
    countries _ = ["PA"]

instance Currency PEN where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "604"
    decimalDigits _ = 2
    symbol _ = "S"
    exchangeUSD _ = 0.31
    countries _ = ["PE"]

instance Currency PYG where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "600"
    decimalDigits _ = 0
    symbol _ = "₲"
    exchangeUSD _ = 0.000179
    countries _ = ["PY"]

instance Currency SIM where
    currencyType _ = Fictional
    isoCode = show
    isoNumericCode _ = ""
    decimalDigits _ = 0
    symbol _ = "S"
    exchangeUSD _ = 10 -- Very rough approximation
    countries _ = []

instance Currency USD where
    currencyType _ = Circulating
    isoCode = show
    isoNumericCode _ = "840"
    decimalDigits _ = 2
    symbol _ = "$"
    exchangeUSD _ = 1.0
    countries _ = ["US", "AS", "BB", "BM", "IO", "VG", "BQ", "EC", "MH"
        , "FM", "MP", "PW", "PA", "PR", "TL", "TC", "VI"]
