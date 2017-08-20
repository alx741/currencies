[![CircleCI](https://circleci.com/gh/alx741/currencies.svg?style=svg)](https://circleci.com/gh/alx741/currencies)

# currencies

ISO 4217 Currencies representation, pretty printing and conversion:

* Represent monetary amounts of a particular currency in a type-safe manner
* Convert amounts between different currencies
* Print human readable amounts

```haskell
prettyPrint (Amount USD 2342.2)
-- "USD 2,342.20"

prettyPrint (Amount EUR 45827.346)
-- "UER 45,827.35"

prettyPrintWith (defaultConfig { useCurrencySymbol = True }) (Amount USD 2342.2)
-- "$ a,342.20"

prettyPrintWith (defaultConfig { useCurrencySymbol = True }) (Amount EUR 2342.2)
-- "€ 2,342.20"

prettyPrintWith (defaultConfig { showDecimals = False }) (Amount USD 25.50)
-- "USD 25"

convert USD (Amount EUR 23482.34)
-- Amount USD 27709.1612

prettyPrint $ convert USD (Amount EUR 23482.34)
-- "USD 27,709.16"

compareAmounts  (Amount EUR 1000) (Amount BTC 1)
-- LT
```
