module Data.Currency.Pretty
    ( -- * Configuration
      PrettyConfig(..)
    , defaultConfig

      -- * Pretty printing
    , prettyPrint
    , prettyPrintWith
    ) where

import Text.Printf
import Data.Currency.Currencies
import Data.Monoid ((<>))

-- | Pretty print a monetary amount using 'defaultConfig'
prettyPrint :: (Currency c) => Amount c -> String
prettyPrint = prettyPrintWith defaultConfig

-- | Pretty print a monetary amount with a custom configuration
prettyPrintWith :: (Currency c) => PrettyConfig -> Amount c -> String
prettyPrintWith cnf (Amount currency amount) =
    prefixSymbol currency cnf
    $ prefixCode currency cnf
    $ changeDecimalSep currency cnf
    $ largeAmountSeparate currency cnf
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

largeAmountSeparate :: (Currency c) => c -> PrettyConfig -> String -> String
largeAmountSeparate currency cnf amount
    | separateFourDigitAmounts cnf = separated ++ decimal
    | otherwise = if length integer <= 4 then amount else separated ++ decimal
    where
        (integer, decimal) = span (/= '.') amount
        separated = reverse $ intersperseN 3 (largeAmountSeparator cnf) $ reverse integer

toDecimalString :: (Currency c) => c -> PrettyConfig -> Double -> String
toDecimalString currency cnf amount
    | showDecimals cnf = printf format amount
    | otherwise = takeWhile (/= '.') $ printf "%.1f" amount
    where format = "%." <> (show $ decimalDigits currency) <> "f"

intersperseN :: Eq a => Int -> a -> [a] -> [a]
intersperseN n s ss
    | remainder == [] = ss
    | otherwise = (take ++ [s]) ++ intersperseN n s remainder
    where (take, remainder) = splitAt n ss


data PrettyConfig = PrettyConfig
    { showDecimals :: Bool
    , separateFourDigitAmounts :: Bool
    , useCurrencySymbol :: Bool
    , suffixIsoCode :: Bool
    , largeAmountSeparator :: Char
    , decimalSeparator :: Char
    } deriving (Show)


defaultConfig :: PrettyConfig
defaultConfig = PrettyConfig
    { showDecimals = True
    , separateFourDigitAmounts = False
    , useCurrencySymbol = False
    , suffixIsoCode = False
    , largeAmountSeparator = ','
    , decimalSeparator = '.'
    }
