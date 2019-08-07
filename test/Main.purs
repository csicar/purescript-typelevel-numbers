module Test.Main where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Prim.Symbol (class Cons)
import Type.Data.Int (IProxy, Neg, Pos, intSignedConvert, inverse, reflectInt, sumInt)
import Type.Data.Internal.Num.Reps (type (:*), D1, D9, D2, NumProxy)
import Type.Data.Nat (NProxy, addT, divMod10, mirrorSymbol, order, subT, succ, toDigits, undef)
import Type.Data.Ordering (OProxy(..), GT, EQ, LT)


-- convertion


testToDigits :: _
testToDigits = toDigits (undef :: SProxy "123")

testToDigits2 :: NumProxy (D1 :* D9 :* D2)
testToDigits2 = toDigits (undef :: SProxy _)

--cons
cons :: ∀ a b c. Cons a b c => NProxy c -> Tuple (NProxy a) (NProxy b)
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

testDivMod10 :: Tuple (NProxy "1") (NProxy "0")
testDivMod10 = divMod10 (undef :: NProxy "10")

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

testSucc7 :: NProxy "101"
testSucc7 = succ (undef :: NProxy "100")

-- Add
testAdd1 :: NProxy "1"
testAdd1 = addT (undef :: NProxy "0") (undef :: NProxy "1")

testAdd2 :: NProxy "10"
testAdd2 = addT (undef :: NProxy "5") (undef :: NProxy "5")

testAdd3 :: NProxy "20"
testAdd3 = addT (undef :: NProxy "10") (undef :: NProxy "10")

testAdd4 :: NProxy "99"
testAdd4 = addT (undef :: NProxy "77") (undef :: NProxy "22")

testAdd5 :: NProxy _
testAdd5 = addT (undef :: NProxy "12") (undef :: NProxy "72")

testAdd6 :: NProxy "9"
testAdd6 = addT (undef :: NProxy _) (undef :: NProxy "2")

testAdd7 :: NProxy "100"
testAdd7 = addT (undef :: NProxy "51") (undef :: NProxy "49")

testAdd8 :: NProxy "10"
testAdd8 = addT (undef :: NProxy _) (undef :: NProxy "9")

testAdd9 :: NProxy "12"
testAdd9 = addT (undef :: NProxy _) (undef :: NProxy "8")

testAdd10 :: NProxy "12"
testAdd10 = addT (undef :: NProxy "8") (undef :: NProxy _)

testAdd11 :: NProxy "10"
testAdd11 = addT (undef :: NProxy _) (undef :: NProxy "0")

testAdd12 :: NProxy "10"
testAdd12 = addT (undef :: NProxy "10") (undef :: NProxy "0")

testAdd13 :: NProxy "10"
testAdd13 = addT (undef :: NProxy _) (undef :: NProxy "0")

testAdd14 :: NProxy "10"
testAdd14 = addT (undef :: NProxy "10") (undef :: NProxy "0")

testAdd15 :: NProxy "40"
testAdd15 = addT (undef :: NProxy _) (undef :: NProxy "10")

-- Sub
testSub1 :: NProxy "2"
testSub1 = subT (undef :: NProxy "10") (undef :: NProxy "8")

testSub2 :: NProxy "4"
testSub2 = subT (undef :: NProxy "12") (undef :: NProxy "8")

testSub3 :: NProxy _
testSub3 = subT (undef :: NProxy "12") (undef :: NProxy "8")

testSub4 :: NProxy "42"
testSub4 = subT (undef :: NProxy "50") (undef :: NProxy "8")

-- Order
testOrder :: OProxy GT
testOrder = order (undef :: NProxy "5") (undef :: NProxy "2")

testOrder2 :: OProxy EQ
testOrder2 = order (undef :: NProxy "50") (undef :: NProxy "50")

testOrder3 :: OProxy LT
testOrder3 = order (undef :: NProxy "5") (undef :: NProxy "18")

testOrder4 :: OProxy GT
testOrder4 = order (undef :: NProxy "15") (undef :: NProxy "2")

testOrder5 :: OProxy _
testOrder5 = order (undef :: NProxy "5") (undef :: NProxy "18")

testOrder6 :: OProxy _
testOrder6 = order (undef :: NProxy "15") (undef :: NProxy "2")

-- test Int reflect
testReflectInt1 :: Int
testReflectInt1 = reflectInt (undef :: IProxy "12")

testReflectInt2 :: Int
testReflectInt2 = reflectInt (undef :: IProxy "-12")

-- extract int

textExtract :: Pos "12"
textExtract = intSignedConvert (undef :: IProxy "12")

textExtract2 :: Neg "12"
textExtract2 = intSignedConvert (undef :: IProxy "-12")

textExtract3 :: Pos "0"
textExtract3 = intSignedConvert (undef :: IProxy "0")

textExtract4 :: _
textExtract4 = intSignedConvert (undef :: IProxy "-2")

textExtract5 :: Pos "2"
textExtract5 = intSignedConvert (undef :: IProxy _)

-- inverse

testInverse :: IProxy "0"
testInverse = inverse (undef :: IProxy "0")

testInverse2 :: IProxy "10"
testInverse2 = inverse (undef :: IProxy "-10")

testInverse3 :: IProxy _
testInverse3 = inverse (undef :: IProxy "-10")

testInverse4 :: IProxy "10"
testInverse4 = inverse (undef :: IProxy _)

testInverse5 :: IProxy "-2"
testInverse5 = inverse (undef :: IProxy "2")

testInverse6 :: IProxy "2"
testInverse6 = inverse (undef :: IProxy "-2")

-- sum
testSum1 :: IProxy "2"
testSum1 = sumInt (undef :: IProxy "1") (undef :: IProxy "1")

testSum2 :: IProxy "3"
testSum2 = sumInt (undef :: IProxy "1") (undef :: IProxy "2")

testSum3 :: IProxy "1"
testSum3 = sumInt (undef :: IProxy "-1") (undef :: IProxy "2")

testSum4 :: IProxy "-2"
testSum4 = sumInt (undef :: IProxy "-7") (undef :: IProxy "5")

testSum5 :: IProxy "-2"
testSum5 = sumInt (undef :: IProxy "1") (undef :: IProxy "-3")

testSum6 :: IProxy "4"
testSum6 = sumInt (undef :: IProxy "-5") (undef :: IProxy "9")

testSum7 :: IProxy _
testSum7 = sumInt (undef :: IProxy "-5") (undef :: IProxy "9")

testSum8 :: IProxy "-1"
testSum8 = sumInt (undef :: IProxy "1") (undef :: IProxy "-2")

testSum9 :: IProxy "-1"
testSum9 = sumInt (undef :: IProxy "-2") (undef :: IProxy "1")

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
