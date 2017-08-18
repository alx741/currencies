module Data.Currency.Pretty where

import Data.Text
import Data.Currency.Currencies

-- | Pretty print a monetary amount using 'defaultConfig'
humanReadable :: (IsCurrency c) => Amount c -> Text
humanReadable = humanReadableWith defaultConfig

-- | Pretty print a monetary amount with a custom configuration
humanReadableWith :: (IsCurrency c) => PrettyConfig -> Amount c -> Text
humanReadableWith cnf amount = pack $ show amount


defaultConfig :: PrettyConfig
defaultConfig = PrettyConfig
    { showFractions = True
    , separateFourDigitAmounts = False
    , useCurrencySymbol = False
    , suffixIsoCode = False
    , largeAmountSeparator = ','
    , decimalSeparator = '.'
    }

data PrettyConfig = PrettyConfig
    { showFractions :: Bool
    , separateFourDigitAmounts :: Bool
    , useCurrencySymbol :: Bool
    , suffixIsoCode :: Bool
    , largeAmountSeparator :: Char
    , decimalSeparator :: Char
    } deriving (Show)
