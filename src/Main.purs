module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Tuple (Tuple(Tuple))
import Prim.Symbol (class Cons, class Append)
import Type.Prelude (kind Boolean, True, False)
import Unsafe.Coerce (unsafeCoerce)

foreign import data NProxy :: Symbol -> Type



undef :: ∀a. a
undef = unsafeCoerce unit

class MirrorUnidirectional (a :: Symbol) (mirror :: Symbol) | a -> mirror, mirror -> a

instance mirrorEmpty :: MirrorUnidirectional "" ""
else
instance mirrorCons :: (Cons head tail sym, MirrorUnidirectional tailMirror tail, MirrorUnidirectional tail tailMirror, Append tailMirror head mirror) => MirrorUnidirectional sym mirror
else
instance mirrorCons' :: (Cons head tail sym, MirrorUnidirectional tail tailMirror, Append tailMirror head mirror) => MirrorUnidirectional mirror sym

class Mirror (a :: Symbol) (mirror :: Symbol) | a -> mirror, mirror -> a

instance mirror :: (MirrorUnidirectional a b, MirrorUnidirectional b a) => Mirror a b

mirrorSymbol :: ∀a b. Mirror a b => NProxy a -> NProxy b
mirrorSymbol = unsafeCoerce


class Snoc (head :: Symbol) (tail :: Symbol) (symbol :: Symbol) | head tail -> symbol, tail symbol -> head, head symbol -> tail, symbol -> head tail

instance snocMirror :: (Mirror sym mirror, Cons head tailMirror mirror, Mirror tailMirror tail) => Snoc head tail sym

class IsNat (a :: Symbol) where
  reflectNat :: NProxy a -> Int

instance isNat0 ∷ IsNat "0" where reflectNat _ = 0
else instance isNat1 ∷ IsNat "1" where reflectNat _ = 1
else instance isNat2 ∷ IsNat "2" where reflectNat _ = 2
else instance isNat3 ∷ IsNat "3" where reflectNat _ = 3
else instance isNat4 ∷ IsNat "4" where reflectNat _ = 4
else instance isNat5 ∷ IsNat "5" where reflectNat _ = 5
else instance isNat6 ∷ IsNat "6" where reflectNat _ = 6
else instance isNat7 ∷ IsNat "7" where reflectNat _ = 7
else instance isNat8 ∷ IsNat "8" where reflectNat _ = 8
else instance isNat9 ∷ IsNat "9" where reflectNat _ = 9
else instance isNatDigits :: (Snoc head tail sym, IsNat head, IsNat tail) => IsNat sym where
  reflectNat _ = (reflectNat (un :: NProxy head)) + 10*reflectNat (un :: NProxy tail)
    where 
      un :: ∀a. a
      un = unsafeCoerce unit


instance divMod1000       :: DivMod10 "0" "0" "0"
else instance divMod10D10 :: DivMod10 "1" "0" "1"
else instance divMod10D20 :: DivMod10 "2" "0" "2"
else instance divMod10D30 :: DivMod10 "3" "0" "3"
else instance divMod10D40 :: DivMod10 "4" "0" "4"
else instance divMod10D50 :: DivMod10 "5" "0" "5"
else instance divMod10D60 :: DivMod10 "6" "0" "6"
else instance divMod10D70 :: DivMod10 "7" "0" "7"
else instance divMod10D80 :: DivMod10 "8" "0" "8"
else instance divMod10D90 :: DivMod10 "9" "0" "9"
else instance divMod10Rest :: (Snoc x tail head) => DivMod10 head tail x

class DivMod10 (x :: Symbol) (h :: Symbol) (l :: Symbol) | x -> h l, h l -> x

divMod10 :: ∀x r q. DivMod10 x r q => NProxy x -> Tuple (NProxy r) (NProxy q)
divMod10 _ = Tuple undef undef


class (IsNat a, IsNat b) <= Succ a b | a -> b, b -> a

class (IsNat a) <= IsZero (a :: Symbol) (isZero :: Boolean) | a -> isZero

instance isZero :: IsZero "0" True
else instance isNotZero :: IsNat l => IsZero l False

class (IsNat a) <= IsPos a

instance nonZeroPos :: (IsNat a, IsZero a False) => IsPos a

-- -- instance typelevelSucc :: (Pos y, IsZero y yz, DivMod10 x xi xl, SuccP xi xl yi yl yz, DivMod10 y yi yl) => Succ x y
instance succDivMod :: (IsZero y yz, DivMod10 x xi xl, SuccP xi xl yi yl yz, DivMod10 y yi yl, IsNat y, IsNat x) => Succ x y

class SuccP (xh :: Symbol) (xl :: Symbol) (yh :: Symbol) (yl :: Symbol) (yz :: Boolean) | xh xl -> yh yl yz, yh yl yz -> xh xl

class Failure t
data PredecessorOfZeroError t

instance failurePredOfZeroError :: Failure (PredecessorOfZeroError x) => SuccP ("error") ("error") "0" "0" True
else instance succPxiD0xiD1 :: SuccP xi "0" xi "1" False
else instance succPxiD1xiD2 :: SuccP xi "1" xi "2" False
else instance succPxiD2xiD3 :: SuccP xi "2" xi "3" False
else instance succPxiD3xiD4 :: SuccP xi "3" xi "4" False
else instance succPxiD4xiD5 :: SuccP xi "4" xi "5" False
else instance succPxiD5xiD6 :: SuccP xi "5" xi "6" False
else instance succPxiD6xiD7 :: SuccP xi "6" xi "7" False
else instance succPxiD7xiD8 :: SuccP xi "7" xi "8" False
else instance succPxiD8xiD9 :: SuccP xi "8" xi "9" False
else instance succPxiD9iyD0 :: Succ xi yi => SuccP xi "9" yi "0" False

succ :: forall x y. Succ x y => NProxy x -> NProxy y
succ _ = undef

class IsPos x <= Pred x (y :: Symbol) | x -> y, y -> x
instance succPred :: (IsNat y, Succ x y) => Pred y x

pred :: forall x y. Pred x y => NProxy x -> NProxy y
pred _ = undef


class IsNat x <= AddP (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z, z x -> y
-- shortcuts
instance sumNat00 :: AddP "0" "0" "0"
else instance sumNat01 :: AddP "0" "1" "1"
else instance sumNat02 :: AddP "0" "2" "2"
else instance sumNat03 :: AddP "0" "3" "3"
else instance sumNat04 :: AddP "0" "4" "4"
else instance sumNat05 :: AddP "0" "5" "5"
else instance sumNat06 :: AddP "0" "6" "6"
else instance sumNat07 :: AddP "0" "7" "7"
else instance sumNat08 :: AddP "0" "8" "8"
else instance sumNat09 :: AddP "0" "9" "9"
else instance sumNat10 :: AddP "1" "0" "1"
else instance sumNat11 :: AddP "1" "1" "2"
else instance sumNat12 :: AddP "1" "2" "3"
else instance sumNat13 :: AddP "1" "3" "4"
else instance sumNat14 :: AddP "1" "4" "5"
else instance sumNat15 :: AddP "1" "5" "6"
else instance sumNat16 :: AddP "1" "6" "7"
else instance sumNat17 :: AddP "1" "7" "8"
else instance sumNat18 :: AddP "1" "8" "9"
else instance sumNat19 :: AddP "1" "9" "10"
else instance sumNat20 :: AddP "2" "0" "2"
else instance sumNat21 :: AddP "2" "1" "3"
else instance sumNat22 :: AddP "2" "2" "4"
else instance sumNat23 :: AddP "2" "3" "5"
else instance sumNat24 :: AddP "2" "4" "6"
else instance sumNat25 :: AddP "2" "5" "7"
else instance sumNat26 :: AddP "2" "6" "8"
else instance sumNat27 :: AddP "2" "7" "9"
else instance sumNat28 :: AddP "2" "8" "10"
else instance sumNat29 :: AddP "2" "9" "11"
else instance sumNat30 :: AddP "3" "0" "3"
else instance sumNat31 :: AddP "3" "1" "4"
else instance sumNat32 :: AddP "3" "2" "5"
else instance sumNat33 :: AddP "3" "3" "6"
else instance sumNat34 :: AddP "3" "4" "7"
else instance sumNat35 :: AddP "3" "5" "8"
else instance sumNat36 :: AddP "3" "6" "9"
else instance sumNat37 :: AddP "3" "7" "10"
else instance sumNat38 :: AddP "3" "8" "11"
else instance sumNat39 :: AddP "3" "9" "12"
else instance sumNat40 :: AddP "4" "0" "4"
else instance sumNat41 :: AddP "4" "1" "5"
else instance sumNat42 :: AddP "4" "2" "6"
else instance sumNat43 :: AddP "4" "3" "7"
else instance sumNat44 :: AddP "4" "4" "8"
else instance sumNat45 :: AddP "4" "5" "9"
else instance sumNat46 :: AddP "4" "6" "10"
else instance sumNat47 :: AddP "4" "7" "11"
else instance sumNat48 :: AddP "4" "8" "12"
else instance sumNat49 :: AddP "4" "9" "13"
else instance sumNat50 :: AddP "5" "0" "5"
else instance sumNat51 :: AddP "5" "1" "6"
else instance sumNat52 :: AddP "5" "2" "7"
else instance sumNat53 :: AddP "5" "3" "8"
else instance sumNat54 :: AddP "5" "4" "9"
else instance sumNat55 :: AddP "5" "5" "10"
else instance sumNat56 :: AddP "5" "6" "11"
else instance sumNat57 :: AddP "5" "7" "12"
else instance sumNat58 :: AddP "5" "8" "13"
else instance sumNat59 :: AddP "5" "9" "14"
else instance sumNat60 :: AddP "6" "0" "6"
else instance sumNat61 :: AddP "6" "1" "7"
else instance sumNat62 :: AddP "6" "2" "8"
else instance sumNat63 :: AddP "6" "3" "9"
else instance sumNat64 :: AddP "6" "4" "10"
else instance sumNat65 :: AddP "6" "5" "11"
else instance sumNat66 :: AddP "6" "6" "12"
else instance sumNat67 :: AddP "6" "7" "13"
else instance sumNat68 :: AddP "6" "8" "14"
else instance sumNat69 :: AddP "6" "9" "15"
else instance sumNat70 :: AddP "7" "0" "7"
else instance sumNat71 :: AddP "7" "1" "8"
else instance sumNat72 :: AddP "7" "2" "9"
else instance sumNat73 :: AddP "7" "3" "10"
else instance sumNat74 :: AddP "7" "4" "11"
else instance sumNat75 :: AddP "7" "5" "12"
else instance sumNat76 :: AddP "7" "6" "13"
else instance sumNat77 :: AddP "7" "7" "14"
else instance sumNat78 :: AddP "7" "8" "15"
else instance sumNat79 :: AddP "7" "9" "16"
else instance sumNat80 :: AddP "8" "0" "8"
else instance sumNat81 :: AddP "8" "1" "9"
else instance sumNat82 :: AddP "8" "2" "10"
else instance sumNat83 :: AddP "8" "3" "11"
else instance sumNat84 :: AddP "8" "4" "12"
else instance sumNat85 :: AddP "8" "5" "13"
else instance sumNat86 :: AddP "8" "6" "14"
else instance sumNat87 :: AddP "8" "7" "15"
else instance sumNat88 :: AddP "8" "8" "16"
else instance sumNat89 :: AddP "8" "9" "17"
else instance sumNat90 :: AddP "9" "0" "9"
else instance sumNat91 :: AddP "9" "1" "10"
else instance sumNat92 :: AddP "9" "2" "11"
else instance sumNat93 :: AddP "9" "3" "12"
else instance sumNat94 :: AddP "9" "4" "13"
else instance sumNat95 :: AddP "9" "5" "14"
else instance sumNat96 :: AddP "9" "6" "15"
else instance sumNat97 :: AddP "9" "7" "16"
else instance sumNat98 :: AddP "9" "8" "17"
else instance sumNat99 :: AddP "9" "9" "18"

else instance addPD0ToNat :: IsNat y => AddP "0" y y
else instance addPD1ToSucc :: Succ y z => AddP "1" y z
else instance addPD2ToSucc :: (Succ z z', AddP "1" y z) => AddP "2" y z'
else instance addPD3ToSucc :: (Succ z z', AddP "2" y z) => AddP "3" y z'
else instance addPD4ToSucc :: (Succ z z', AddP "3" y z) => AddP "4" y z'
else instance addPD5ToSucc :: (Succ z z', AddP "4" y z) => AddP "5" y z'
else instance addPD6ToSucc :: (Succ z z', AddP "5" y z) => AddP "6" y z'
else instance addPD7ToSucc :: (Succ z z', AddP "6" y z) => AddP "7" y z'
else instance addPD8ToSucc :: (Succ z z', AddP "7" y z) => AddP "8" y z'
else instance addPD9ToSucc :: (Succ z z', AddP "8" y z) => AddP "9" y z'
-- else instance addPMultiDigits :: (Pos (xi :* xl), IsNat z, AddP xi yi zi, DivMod10 y yi yl, AddP xl (zi :* yl) z) => AddP (xi :* xl) y z
else instance addPMultiDigits :: (IsPos x, IsNat x, DivMod10 x xi xl, DivMod10 y yi yl, AddP xi yi zi, Append zi yl ziyl, Append xi xl xixl, AddP xl ziyl z) => AddP x y z



class (AddP x y z, AddP y x z) <= Add x y z | x y -> z, z x -> y, z y -> x
instance addTypeLevelRelation :: (AddP x y z, AddP y x z) => Add x y z

addT :: forall x y z. (Add x y z) => NProxy x -> NProxy y -> NProxy z
addT _ _ = undef

class Sub (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z, z x -> y, z y -> x
instance subtractTypeLevelRelation :: Add x y z => Sub z y x

subT :: forall x y z. (Sub x y z) => NProxy x -> NProxy y -> NProxy z
subT _ _ = undef

-- class (IsNat a, IsNat b, IsNat c) <= SumNat (a :: Symbol) (b :: Symbol) (c :: Symbol) | a b -> c


-- putStrLn $ unlines $ map (intercalate " ") $ map (map show) $ map (\(a,b) -> [a, b, a+b]) $ [(a, b) | a <- [0..9], b <- [0..9]]
-- instance sumNat00 :: SumNat "0" "0" "0"
-- instance sumNat01 :: SumNat "0" "1" "1"
-- instance sumNat02 :: SumNat "0" "2" "2"
-- instance sumNat03 :: SumNat "0" "3" "3"
-- instance sumNat04 :: SumNat "0" "4" "4"
-- instance sumNat05 :: SumNat "0" "5" "5"
-- instance sumNat06 :: SumNat "0" "6" "6"
-- instance sumNat07 :: SumNat "0" "7" "7"
-- instance sumNat08 :: SumNat "0" "8" "8"
-- instance sumNat09 :: SumNat "0" "9" "9"
-- instance sumNat10 :: SumNat "1" "0" "1"
-- instance sumNat11 :: SumNat "1" "1" "2"
-- instance sumNat12 :: SumNat "1" "2" "3"
-- instance sumNat13 :: SumNat "1" "3" "4"
-- instance sumNat14 :: SumNat "1" "4" "5"
-- instance sumNat15 :: SumNat "1" "5" "6"
-- instance sumNat16 :: SumNat "1" "6" "7"
-- instance sumNat17 :: SumNat "1" "7" "8"
-- instance sumNat18 :: SumNat "1" "8" "9"
-- instance sumNat19 :: SumNat "1" "9" "10"
-- instance sumNat20 :: SumNat "2" "0" "2"
-- instance sumNat21 :: SumNat "2" "1" "3"
-- instance sumNat22 :: SumNat "2" "2" "4"
-- instance sumNat23 :: SumNat "2" "3" "5"
-- instance sumNat24 :: SumNat "2" "4" "6"
-- instance sumNat25 :: SumNat "2" "5" "7"
-- instance sumNat26 :: SumNat "2" "6" "8"
-- instance sumNat27 :: SumNat "2" "7" "9"
-- instance sumNat28 :: SumNat "2" "8" "10"
-- instance sumNat29 :: SumNat "2" "9" "11"
-- instance sumNat30 :: SumNat "3" "0" "3"
-- instance sumNat31 :: SumNat "3" "1" "4"
-- instance sumNat32 :: SumNat "3" "2" "5"
-- instance sumNat33 :: SumNat "3" "3" "6"
-- instance sumNat34 :: SumNat "3" "4" "7"
-- instance sumNat35 :: SumNat "3" "5" "8"
-- instance sumNat36 :: SumNat "3" "6" "9"
-- instance sumNat37 :: SumNat "3" "7" "10"
-- instance sumNat38 :: SumNat "3" "8" "11"
-- instance sumNat39 :: SumNat "3" "9" "12"
-- instance sumNat40 :: SumNat "4" "0" "4"
-- instance sumNat41 :: SumNat "4" "1" "5"
-- instance sumNat42 :: SumNat "4" "2" "6"
-- instance sumNat43 :: SumNat "4" "3" "7"
-- instance sumNat44 :: SumNat "4" "4" "8"
-- instance sumNat45 :: SumNat "4" "5" "9"
-- instance sumNat46 :: SumNat "4" "6" "10"
-- instance sumNat47 :: SumNat "4" "7" "11"
-- instance sumNat48 :: SumNat "4" "8" "12"
-- instance sumNat49 :: SumNat "4" "9" "13"
-- instance sumNat50 :: SumNat "5" "0" "5"
-- instance sumNat51 :: SumNat "5" "1" "6"
-- instance sumNat52 :: SumNat "5" "2" "7"
-- instance sumNat53 :: SumNat "5" "3" "8"
-- instance sumNat54 :: SumNat "5" "4" "9"
-- instance sumNat55 :: SumNat "5" "5" "10"
-- instance sumNat56 :: SumNat "5" "6" "11"
-- instance sumNat57 :: SumNat "5" "7" "12"
-- instance sumNat58 :: SumNat "5" "8" "13"
-- instance sumNat59 :: SumNat "5" "9" "14"
-- instance sumNat60 :: SumNat "6" "0" "6"
-- instance sumNat61 :: SumNat "6" "1" "7"
-- instance sumNat62 :: SumNat "6" "2" "8"
-- instance sumNat63 :: SumNat "6" "3" "9"
-- instance sumNat64 :: SumNat "6" "4" "10"
-- instance sumNat65 :: SumNat "6" "5" "11"
-- instance sumNat66 :: SumNat "6" "6" "12"
-- instance sumNat67 :: SumNat "6" "7" "13"
-- instance sumNat68 :: SumNat "6" "8" "14"
-- instance sumNat69 :: SumNat "6" "9" "15"
-- instance sumNat70 :: SumNat "7" "0" "7"
-- instance sumNat71 :: SumNat "7" "1" "8"
-- instance sumNat72 :: SumNat "7" "2" "9"
-- instance sumNat73 :: SumNat "7" "3" "10"
-- instance sumNat74 :: SumNat "7" "4" "11"
-- instance sumNat75 :: SumNat "7" "5" "12"
-- instance sumNat76 :: SumNat "7" "6" "13"
-- instance sumNat77 :: SumNat "7" "7" "14"
-- instance sumNat78 :: SumNat "7" "8" "15"
-- instance sumNat79 :: SumNat "7" "9" "16"
-- instance sumNat80 :: SumNat "8" "0" "8"
-- instance sumNat81 :: SumNat "8" "1" "9"
-- instance sumNat82 :: SumNat "8" "2" "10"
-- instance sumNat83 :: SumNat "8" "3" "11"
-- instance sumNat84 :: SumNat "8" "4" "12"
-- instance sumNat85 :: SumNat "8" "5" "13"
-- instance sumNat86 :: SumNat "8" "6" "14"
-- instance sumNat87 :: SumNat "8" "7" "15"
-- instance sumNat88 :: SumNat "8" "8" "16"
-- instance sumNat89 :: SumNat "8" "9" "17"
-- instance sumNat90 :: SumNat "9" "0" "9"
-- instance sumNat91 :: SumNat "9" "1" "10"
-- instance sumNat92 :: SumNat "9" "2" "11"
-- instance sumNat93 :: SumNat "9" "3" "12"
-- instance sumNat94 :: SumNat "9" "4" "13"
-- instance sumNat95 :: SumNat "9" "5" "14"
-- instance sumNat96 :: SumNat "9" "6" "15"
-- instance sumNat97 :: SumNat "9" "7" "16"
-- instance sumNat98 :: SumNat "9" "8" "17"
-- instance sumNat99 :: SumNat "9" "9" "18"

main :: Effect Unit
main = do
  log "🍝"
