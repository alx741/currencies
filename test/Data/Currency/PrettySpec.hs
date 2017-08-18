module Data.Currency.PrettySpec where

import Test.Hspec
import Data.Currency.Pretty
import Data.Currency.Currencies

spec :: Spec
spec = do
    describe "humanReadable" $ do
        it "prints human readable monetary amounts" $ do
            humanReadable (Amount USD 23.50)  `shouldBe` "USD 23.50"
            humanReadable (Amount USD 540.256)  `shouldBe` "USD 540.26"
            humanReadable (Amount USD 300.251)  `shouldBe` "USD 300.25"
            humanReadable (Amount USD 3.4)  `shouldBe` "USD 3.40"

        it "respects currency decimal digits" $ do
            humanReadable (Amount CLP 345.35)  `shouldBe` "CLP 345"
            humanReadable (Amount CLP 23.53)  `shouldBe` "CLP 24"

        it "can use the currency code as suffix" $ do
            let config = defaultConfig { suffixIsoCode = True }
            humanReadableWith config (Amount USD 23.50)  `shouldBe` "23.50 USD"

