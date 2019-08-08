module Test.Main where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Prim.Symbol (class Cons)
import Type.Data.Int (IProxy(..), Neg, Pos, intSignedConvert, inverse, reflectInt, sumInt)
import Type.Data.Internal.Num.Reps (type (:*), D1, D9, D2, NumProxy)
import Type.Data.Nat (SLProxy, Cons, NProxy(..), Nil, addT, divMod10, mirrorSymbol, order, subT, succ, symbolToList, symbolToSnoc, toDigits, undef)
import Type.Data.Ordering (OProxy(..), GT, EQ, LT)


-- convertion


testToDigits :: _
testToDigits = toDigits (undef :: SProxy "123")

testToDigits2 :: NumProxy (D1 :* D9 :* D2)
testToDigits2 = toDigits (undef :: SProxy _)

--cons
cons :: ∀ a b c. Cons a b c => NProxy c -> Tuple (NProxy a) (NProxy b)
cons _ = Tuple (NProxy::_ a) (NProxy::_ b)

testCons1 :: Tuple (NProxy "2") (NProxy "")
testCons1 = cons (NProxy::_ "2")

testCons2 :: Tuple (NProxy _) (NProxy _)
testCons2 = cons (NProxy::_ "2")

-- Mirror
testMirror1 :: NProxy "321"
testMirror1 = mirrorSymbol (NProxy::_ "123")

testMirror2 :: NProxy _
testMirror2 = mirrorSymbol (NProxy::_ "123")

testMirror3 :: NProxy "1"
testMirror3 = mirrorSymbol (NProxy::_ _)

testMirror4 :: NProxy "10"
testMirror4 = mirrorSymbol (NProxy::_ "01")

testMirror5 :: NProxy _
testMirror5 = mirrorSymbol (NProxy::_ "01")

testMirror6 :: NProxy "10"
testMirror6 = mirrorSymbol (NProxy::_ _)

-- SymbolList
testSymbolList :: SLProxy (Cons "T" (Cons "e" (Cons "s" (Cons "t" Nil))))
testSymbolList = symbolToList (undef :: SProxy "Test")

testSymbolList2 :: SLProxy _
testSymbolList2 = symbolToList (undef :: SProxy "Test")

testSymbolList3 :: SLProxy (Cons "T" (Cons "e" (Cons "s" (Cons "t" Nil))))
testSymbolList3 = symbolToList (undef :: SProxy _)

testConsList :: SLProxy (Cons "d" (Cons "s" (Cons "a" Nil)))
testConsList = symbolToSnoc (undef :: SProxy "asd")

testConsList2 :: SLProxy _
testConsList2 = symbolToSnoc (undef :: SProxy "asd")

testConsList3 :: SLProxy (Cons "d" (Cons "s" (Cons "a" Nil)))
testConsList3 = symbolToSnoc (undef :: SProxy _)

-- DivMod10
testDivMod1 :: Tuple (NProxy "12") (NProxy "3")
testDivMod1 = divMod10 (NProxy::_ "123")

testDivMod2 :: Tuple (NProxy "0") (NProxy "0")
testDivMod2 = divMod10 (NProxy::_ "0")

testDivMod3 :: Tuple (NProxy "0") (NProxy "9")
testDivMod3 = divMod10 (NProxy::_ "9")

testDivMod4 :: Tuple (NProxy "1234") (NProxy "5")
testDivMod4 = divMod10 (NProxy::_ "12345")

testDivMod5 :: Tuple (NProxy "1234") (NProxy "5")
testDivMod5 = divMod10 (NProxy::_ _)

testDivMod6 :: Tuple (NProxy _) (NProxy "5")
testDivMod6 = divMod10 (NProxy::_ "12345")

testDivMod7 :: Tuple (NProxy _) (NProxy _)
testDivMod7 = divMod10 (NProxy::_ "10")

testDivMod8 :: Tuple (NProxy "10") (NProxy "1")
testDivMod8 = divMod10 (NProxy::_ "101")

testDivMod9 :: Tuple (NProxy "10") (NProxy "0")
testDivMod9 = divMod10 (NProxy::_ "100")

testDivMod10 :: Tuple (NProxy "1") (NProxy "0")
testDivMod10 = divMod10 (NProxy::_ "10")

--Succ
testSucc1 :: NProxy "1"
testSucc1 = succ (NProxy::_ "0")

testSucc2 :: NProxy "9"
testSucc2 = succ (NProxy::_ "8")

testSucc3 :: NProxy "10"
testSucc3 = succ (NProxy::_ "9")

testSucc4 :: NProxy "13"
testSucc4 = succ (NProxy::_ "12")

testSucc5 :: NProxy _
testSucc5 = succ (NProxy::_ "12")

testSucc6 :: NProxy "13"
testSucc6 = succ (NProxy::_ _)

testSucc7 :: NProxy "101"
testSucc7 = succ (NProxy::_ "100")

-- Add
testAdd1 :: NProxy "1"
testAdd1 = addT (NProxy::_ "0") (NProxy::_ "1")

testAdd2 :: NProxy "10"
testAdd2 = addT (NProxy::_ "5") (NProxy::_ "5")

testAdd3 :: NProxy "20"
testAdd3 = addT (NProxy::_ "10") (NProxy::_ "10")

testAdd4 :: NProxy "99"
testAdd4 = addT (NProxy::_ "77") (NProxy::_ "22")

testAdd5 :: NProxy _
testAdd5 = addT (NProxy::_ "12") (NProxy::_ "72")

testAdd6 :: NProxy "9"
testAdd6 = addT (NProxy::_ _) (NProxy::_ "2")

testAdd7 :: NProxy "100"
testAdd7 = addT (NProxy::_ "51") (NProxy::_ "49")

testAdd8 :: NProxy "10"
testAdd8 = addT (NProxy::_ _) (NProxy::_ "9")

testAdd9 :: NProxy "12"
testAdd9 = addT (NProxy::_ _) (NProxy::_ "8")

testAdd10 :: NProxy "12"
testAdd10 = addT (NProxy::_ "8") (NProxy::_ _)

testAdd11 :: NProxy "10"
testAdd11 = addT (NProxy::_ _) (NProxy::_ "0")

testAdd12 :: NProxy "10"
testAdd12 = addT (NProxy::_ "10") (NProxy::_ "0")

testAdd13 :: NProxy "10"
testAdd13 = addT (NProxy::_ _) (NProxy::_ "0")

testAdd14 :: NProxy "10"
testAdd14 = addT (NProxy::_ "10") (NProxy::_ "0")

testAdd15 :: NProxy "40"
testAdd15 = addT (NProxy::_ _) (NProxy::_ "10")

-- Sub
testSub1 :: NProxy "2"
testSub1 = subT (NProxy::_ "10") (NProxy::_ "8")

testSub2 :: NProxy "4"
testSub2 = subT (NProxy::_ "12") (NProxy::_ "8")

testSub3 :: NProxy _
testSub3 = subT (NProxy::_ "12") (NProxy::_ "8")

testSub4 :: NProxy "42"
testSub4 = subT (NProxy::_ "50") (NProxy::_ "8")

-- Order
testOrder :: OProxy GT
testOrder = order (NProxy::_ "5") (NProxy::_ "2")

testOrder2 :: OProxy EQ
testOrder2 = order (NProxy::_ "50") (NProxy::_ "50")

testOrder3 :: OProxy LT
testOrder3 = order (NProxy::_ "5") (NProxy::_ "18")

testOrder4 :: OProxy GT
testOrder4 = order (NProxy::_ "15") (NProxy::_ "2")

testOrder5 :: OProxy _
testOrder5 = order (NProxy::_ "5") (NProxy::_ "18")

testOrder6 :: OProxy _
testOrder6 = order (NProxy::_ "15") (NProxy::_ "2")

-- test Int reflect
testReflectInt1 :: Int
testReflectInt1 = reflectInt (IProxy::_ "12")

testReflectInt2 :: Int
testReflectInt2 = reflectInt (IProxy::_ "-12")

-- extract int

textExtract :: Pos "12"
textExtract = intSignedConvert (IProxy::_ "12")

textExtract2 :: Neg "12"
textExtract2 = intSignedConvert (IProxy::_ "-12")

textExtract3 :: Pos "0"
textExtract3 = intSignedConvert (IProxy::_ "0")

textExtract4 :: _
textExtract4 = intSignedConvert (IProxy::_ "-2")

textExtract5 :: Pos "2"
textExtract5 = intSignedConvert (IProxy::_ _)

-- inverse

testInverse :: IProxy "0"
testInverse = inverse (IProxy::_ "0")

testInverse2 :: IProxy "10"
testInverse2 = inverse (IProxy::_ "-10")

testInverse3 :: IProxy _
testInverse3 = inverse (IProxy::_ "-10")

testInverse4 :: IProxy "10"
testInverse4 = inverse (IProxy::_ _)

testInverse5 :: IProxy "-2"
testInverse5 = inverse (IProxy::_ "2")

testInverse6 :: IProxy "2"
testInverse6 = inverse (IProxy::_ "-2")

-- sum
testSum1 :: IProxy "2"
testSum1 = sumInt (IProxy::_ "1") (IProxy::_ "1")

testSum2 :: IProxy "3"
testSum2 = sumInt (IProxy::_ "1") (IProxy::_ "2")

testSum3 :: IProxy "1"
testSum3 = sumInt (IProxy::_ "-1") (IProxy::_ "2")

testSum4 :: IProxy "-2"
testSum4 = sumInt (IProxy::_ "-7") (IProxy::_ "5")

testSum5 :: IProxy "-2"
testSum5 = sumInt (IProxy::_ "1") (IProxy::_ "-3")

testSum6 :: IProxy "4"
testSum6 = sumInt (IProxy::_ "-5") (IProxy::_ "9")

testSum7 :: IProxy _
testSum7 = sumInt (IProxy::_ "-5") (IProxy::_ "9")

testSum8 :: IProxy "-1"
testSum8 = sumInt (IProxy::_ "1") (IProxy::_ "-2")

testSum9 :: IProxy "-1"
testSum9 = sumInt (IProxy::_ "-2") (IProxy::_ "1")

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
