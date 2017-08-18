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
    prefixSymbol currency cnf
    $ prefixCode currency cnf
    $ changeDecimalSep currency cnf
    $ toDecimalString currency cnf amount

prefixSymbol :: (Currency c) => c -> PrettyConfig -> String -> String
prefixSymbol currency cnf val
    | useCurrencySymbol cnf = (symbol currency) <> " " <> val
    | otherwise = val

prefixCode :: (Currency c) => c -> PrettyConfig -> String -> String
prefixCode currency cnf val
    | useCurrencySymbol cnf = val
    | suffixIsoCode cnf = val <> " " <> (isoCode currency)
    | otherwise = (isoCode currency) <> " " <> val

changeDecimalSep :: (Currency c) => c -> PrettyConfig -> String -> String
changeDecimalSep currency cnf val = replaceFst '.' (decimalSeparator cnf) val
    where
        replaceFst :: Char -> Char -> String -> String
        replaceFst c c' [] = []
        replaceFst c c' (s:ss)
            | s == c = c' : ss
            | otherwise = s : replaceFst c c' ss

toDecimalString :: (Currency c) => c -> PrettyConfig -> Double -> String
toDecimalString currency cnf amount
    | showDecimals cnf = printf format amount
    | otherwise = printf "%.0f" amount
    where format = "%." <> (show $ decimalDigits currency) <> "f"


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
