-- | TODO: Pretty print monetary amounts like `$ 34.50`, `USD 3,456.29`,
-- `€ 32 433 938.23`...
-- >>> prettyPrint (Amout USD 2342.2)
-- "USD 2,342.20"
module Data.Currency.Pretty
    ( -- * Pretty printing
      prettyPrint
    , prettyPrintWith

      -- * Configuration
    , PrettyConfig(..)
    , defaultConfig

    , module Data.Currency.Amounts
    ) where

import Text.Printf
import Data.Currency.Amounts
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
    | useCurrencySymbol cnf = symbol currency <> " " <> val
    | otherwise = val

prefixCode :: (Currency c) => c -> PrettyConfig -> String -> String
prefixCode currency cnf val
    | useCurrencySymbol cnf = val
    | suffixIsoCode cnf = val <> " " <> isoCode currency
    | otherwise = isoCode currency <> " " <> val

changeDecimalSep :: (Currency c) => c -> PrettyConfig -> String -> String
changeDecimalSep currency cnf = replaceFst '.' (decimalSeparator cnf)
    where
        replaceFst :: Char -> Char -> String -> String
        replaceFst c c' [] = []
        replaceFst c c' (s:ss)
            | s == c = c' : ss
            | otherwise = s : replaceFst c c' ss

largeAmountSeparate :: (Currency c) => c -> PrettyConfig -> String -> String
largeAmountSeparate currency cnf amount
    | compactFourDigitAmounts cnf = if length integer <= 4 then amount else separated ++ decimal
    | otherwise = separated ++ decimal
    where
        (integer, decimal) = span (/= '.') amount
        separated = reverse $ intersperseN 3 (largeAmountSeparator cnf) $ reverse integer

toDecimalString :: (Currency c) => c -> PrettyConfig -> Double -> String
toDecimalString currency cnf amount
    | showDecimals cnf = printf format amount
    | otherwise = takeWhile (/= '.') $ printf "%.1f" amount
    where format = "%." <> show (decimalDigits currency) <> "f"

intersperseN :: Eq a => Int -> a -> [a] -> [a]
intersperseN n s ss
    | null remainder = ss
    | otherwise = (take ++ [s]) ++ intersperseN n s remainder
    where (take, remainder) = splitAt n ss


data PrettyConfig = PrettyConfig
    { showDecimals :: Bool
    -- | Print four digits amounts as
    -- /USD 1,000,00/ instead of /USD 1000.00/
    , compactFourDigitAmounts :: Bool
    -- | Replace the currency ISO code with its symbol to produce
    -- /$ 23.50/ instead of /USD 23.50/
    , useCurrencySymbol :: Bool
    -- | Use the currency ISO code as suffix to produce
    -- /23.50 USD/ instead of /USD 23.50/
    , suffixIsoCode :: Bool
    , largeAmountSeparator :: Char
    , decimalSeparator :: Char
    } deriving (Show)


defaultConfig :: PrettyConfig
defaultConfig = PrettyConfig
    { showDecimals = True
    , compactFourDigitAmounts = True
    , useCurrencySymbol = False
    , suffixIsoCode = False
    , largeAmountSeparator = ','
    , decimalSeparator = '.'
    }
