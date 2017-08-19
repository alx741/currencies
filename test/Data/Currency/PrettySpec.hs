module Data.Currency.PrettySpec where

import Test.Hspec
import Data.Currency.Pretty
import Data.Currency.Currencies
import Data.Currency.Amounts

spec :: Spec
spec = do
    describe "prettyPrint" $ do
        it "prints human readable monetary amounts" $ do
            prettyPrint (Amount USD 23.50)  `shouldBe` "USD 23.50"
            prettyPrint (Amount USD 540.256)  `shouldBe` "USD 540.26"
            prettyPrint (Amount USD 300.251)  `shouldBe` "USD 300.25"
            prettyPrint (Amount USD 3.4)  `shouldBe` "USD 3.40"
            prettyPrint (Amount EUR 15.589)  `shouldBe` "EUR 15.59"

        it "respects currency decimal digits" $ do
            prettyPrint (Amount CLP 345.35)  `shouldBe` "CLP 345"
            prettyPrint (Amount CLP 23.53)  `shouldBe` "CLP 24"

        it "can use the currency code as suffix" $ do
            let config = defaultConfig { suffixIsoCode = True }
            prettyPrintWith config (Amount USD 23.50)  `shouldBe` "23.50 USD"
            prettyPrintWith config (Amount CLP 345.35)  `shouldBe` "345 CLP"

        it "can omit decimals" $ do
            let config = defaultConfig { showDecimals = False }
            prettyPrintWith config (Amount USD 23.50)  `shouldBe` "USD 23"
            prettyPrintWith config (Amount USD 534.25)  `shouldBe` "USD 534"
            prettyPrintWith config (Amount EUR 15.589)  `shouldBe` "EUR 15"

        it "can use the currency symbol" $ do
            let config = defaultConfig { useCurrencySymbol = True }
            prettyPrintWith config (Amount USD 23.50)  `shouldBe` "$ 23.50"
            prettyPrintWith config (Amount EUR 15.589)  `shouldBe` "€ 15.59"
            prettyPrintWith config (Amount BTC 0.5898)  `shouldBe` "B 0.58980000"

        it "can use a custom decimal separator" $ do
            let config = defaultConfig { decimalSeparator = ',' }
            prettyPrintWith config (Amount USD 23.50)  `shouldBe` "USD 23,50"
            prettyPrintWith config (Amount EUR 15.589)  `shouldBe` "EUR 15,59"
            prettyPrintWith config (Amount USD 23.0)  `shouldBe` "USD 23,00"

        it "uses large amounts separators" $ do
            prettyPrint (Amount USD 32323.50)  `shouldBe` "USD 32,323.50"
            prettyPrint (Amount EUR 3827115.259)  `shouldBe` "EUR 3,827,115.26"

        it "can use a custom large amounts separator" $ do
            let config = defaultConfig { largeAmountSeparator = ' ' }
            prettyPrintWith config (Amount USD 32323.50)  `shouldBe` "USD 32 323.50"
            prettyPrintWith config (Amount EUR 3827115.259)  `shouldBe` "EUR 3 827 115.26"

        it "can avoid separating 4 digit amounts" $ do
            prettyPrint (Amount USD 2323.50)  `shouldBe` "USD 2323.50"
            prettyPrint (Amount EUR 4629.25)  `shouldBe` "EUR 4629.25"
            prettyPrint (Amount USD 23875.00)  `shouldBe` "USD 23,875.00"

        it "can force 4-digit amounts separation" $ do
            let config = defaultConfig { separateFourDigitAmounts = True}
            prettyPrintWith config (Amount USD 2323.50)  `shouldBe` "USD 2,323.50"
            prettyPrintWith config (Amount EUR 4629.25)  `shouldBe` "EUR 4,629.25"
            prettyPrintWith config (Amount USD 23875.00)  `shouldBe` "USD 23,875.00"
