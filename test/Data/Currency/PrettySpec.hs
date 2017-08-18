{-# LANGUAGE OverloadedStrings #-}

module Data.Currency.PrettySpec where

import Test.Hspec
import Data.Currency.Pretty
import Data.Currency.Currencies
import Data.Text

spec :: Spec
spec = do
    describe "humanReadable" $ do
        it "prints human readable monetary amounts" $ do
            humanReadable (amount USD 23.50)  `shouldBe` "$23.50"
