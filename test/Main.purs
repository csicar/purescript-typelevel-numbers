module Test.Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Main (NProxy, addT, divMod10, mirrorSymbol, succ, undef)
import Prim.Symbol (class Cons)

--cons
cons :: ∀a b c. Cons a b c => NProxy c -> Tuple (NProxy a) (NProxy b)
cons _ = Tuple (undef :: NProxy a) (undef :: NProxy b)

testCons1 :: Tuple (NProxy "2") (NProxy "")
testCons1 = cons (undef :: NProxy "2")

testCons2 :: Tuple (NProxy _) (NProxy _)
testCons2 = cons (undef :: NProxy "2")


-- Mirror
testMirror1 :: NProxy "321"
testMirror1 = mirrorSymbol (undef :: NProxy "123")

testMirror2 :: NProxy _
testMirror2 = mirrorSymbol (undef :: NProxy "123")

testMirror3 :: NProxy "1"
testMirror3 = mirrorSymbol (undef :: NProxy _)

testMirror4 :: NProxy "10"
testMirror4 = mirrorSymbol (undef :: NProxy "01")

testMirror5 :: NProxy _
testMirror5 = mirrorSymbol (undef :: NProxy "01")

testMirror6 :: NProxy "10"
testMirror6 = mirrorSymbol (undef :: NProxy _)

-- DivMod10
testDivMod1 :: Tuple (NProxy "12") (NProxy "3")
testDivMod1 = divMod10 (undef :: NProxy "123")

testDivMod2 :: Tuple (NProxy "0") (NProxy "0")
testDivMod2 = divMod10 (undef :: NProxy "0")

testDivMod3 :: Tuple (NProxy "0") (NProxy "9")
testDivMod3 = divMod10 (undef :: NProxy "9")

testDivMod4 :: Tuple (NProxy "1234") (NProxy "5")
testDivMod4 = divMod10 (undef :: NProxy "12345")

testDivMod5 :: Tuple (NProxy "1234") (NProxy "5")
testDivMod5 = divMod10 (undef :: NProxy _)

testDivMod6 :: Tuple (NProxy _) (NProxy "5")
testDivMod6 = divMod10 (undef :: NProxy "12345")

testDivMod7 :: Tuple (NProxy _) (NProxy _)
testDivMod7 = divMod10 (undef :: NProxy "10")

testDivMod8 :: Tuple (NProxy "10") (NProxy "1")
testDivMod8 = divMod10 (undef :: NProxy "101")

testDivMod9 :: Tuple (NProxy "10") (NProxy "0")
testDivMod9 = divMod10 (undef :: NProxy "100")

--Succ
testSucc1 :: NProxy "1"
testSucc1 = succ (undef :: NProxy "0")

testSucc2 :: NProxy "9"
testSucc2 = succ (undef :: NProxy "8")

testSucc3 :: NProxy "10"
testSucc3 = succ (undef :: NProxy "9")

testSucc4 :: NProxy "13"
testSucc4 = succ (undef :: NProxy "12")

testSucc5 :: NProxy _
testSucc5 = succ (undef :: NProxy "12")

testSucc6 :: NProxy "13"
testSucc6 = succ (undef :: NProxy _)

-- Add
testAdd1 :: NProxy "1"
testAdd1 = addT (undef :: NProxy "0") (undef :: NProxy "1")

testAdd2 :: NProxy "10"
testAdd2 = addT (undef :: NProxy "5") (undef :: NProxy "5")

testAdd3 :: NProxy "20"
testAdd3 = addT (undef :: NProxy "10") (undef :: NProxy "10")

testAdd4 :: NProxy "99"
testAdd4 = addT (undef :: NProxy "77") (undef :: NProxy "22")

testAdd5 :: NProxy "99"
testAdd5 = addT (undef :: NProxy _) (undef :: NProxy "22")

testAdd6 :: NProxy "99"
testAdd6 = addT (undef :: NProxy _) (undef :: NProxy "22")

-- testAdd7 :: NProxy "123"
-- testAdd7 = addT (undef :: NProxy "52") (undef :: NProxy "71")

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
