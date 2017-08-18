module Data.Currency.Pretty where

import Text.Printf
import Data.Currency.Currencies
import Data.Monoid ((<>))

-- | Pretty print a monetary amount using 'defaultConfig'
humanReadable :: (Currency c) => Amount c -> String
humanReadable = humanReadableWith defaultConfig

-- | Pretty print a monetary amount with a custom configuration
humanReadableWith :: (Currency c) => PrettyConfig -> Amount c -> String
humanReadableWith cnf (Amount currency amount) =
    prefixCode currency (suffixIsoCode cnf) $ toDecimalString currency (showDecimals cnf) amount

prefixCode :: (Currency c) => c -> Bool -> String -> String
prefixCode currency False val = (isoCode currency) <> " " <> val
prefixCode currency True val = val <> " " <> (isoCode currency)

toDecimalString :: (Currency c) => c -> Bool -> Double -> String
toDecimalString currency True amount = printf format amount
    where format = "%." <> (show $ decimalDigits currency) <> "f"
toDecimalString currency False amount = printf "%.0f" amount


defaultConfig :: PrettyConfig
defaultConfig = PrettyConfig
    { showDecimals = True
    , separateFourDigitAmounts = False
    , useCurrencySymbol = False
    , suffixIsoCode = False
    , largeAmountSeparator = ','
    , decimalSeparator = '.'
    }

data PrettyConfig = PrettyConfig
    { showDecimals :: Bool
    , separateFourDigitAmounts :: Bool
    , useCurrencySymbol :: Bool
    , suffixIsoCode :: Bool
    , largeAmountSeparator :: Char
    , decimalSeparator :: Char
    } deriving (Show)
