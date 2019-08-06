module Test.Main where

import Main
import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

testDivMod1 :: Tuple (NProxy "12") (NProxy "3")
testDivMod1 = divMod10 (undef :: NProxy "123")

testDivMod2 :: Tuple (NProxy "0") (NProxy "0")
testDivMod2 = divMod10 (undef :: NProxy "0")

testDivMod3 :: Tuple (NProxy "0") (NProxy "9")
testDivMod3 = divMod10 (undef :: NProxy "9")

testDivMod4 :: Tuple (NProxy "1234") (NProxy "5")
testDivMod4 = divMod10 (undef :: NProxy "12345")


main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
