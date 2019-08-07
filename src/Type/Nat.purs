module Nat where

import Prelude

import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Console (log)
import Prim.Symbol (class Cons, class Append)
import Type.Data.Ordering (EQ, GT, LT, OProxy(..), kind Ordering)
import Type.Data.Ordering as O
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

class IsPos x <= Pred (x :: Symbol) (y :: Symbol) | x -> y, y -> x
instance succPred :: (IsNat y, Succ x y) => Pred y x

pred :: forall x y. Pred x y => NProxy x -> NProxy y
pred _ = undef


class IsNat x <= AddP (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z, z x -> y
-- shortcuts
-- instance sumNat00 :: AddP "0" "0" "0"
-- else instance sumNat01 :: AddP "0" "1" "1"
-- else instance sumNat02 :: AddP "0" "2" "2"
-- else instance sumNat03 :: AddP "0" "3" "3"
-- else instance sumNat04 :: AddP "0" "4" "4"
-- else instance sumNat05 :: AddP "0" "5" "5"
-- else instance sumNat06 :: AddP "0" "6" "6"
-- else instance sumNat07 :: AddP "0" "7" "7"
-- else instance sumNat08 :: AddP "0" "8" "8"
-- else instance sumNat09 :: AddP "0" "9" "9"
-- else instance sumNat10 :: AddP "1" "0" "1"
-- else instance sumNat11 :: AddP "1" "1" "2"
-- else instance sumNat12 :: AddP "1" "2" "3"
-- else instance sumNat13 :: AddP "1" "3" "4"
-- else instance sumNat14 :: AddP "1" "4" "5"
-- else instance sumNat15 :: AddP "1" "5" "6"
-- else instance sumNat16 :: AddP "1" "6" "7"
-- else instance sumNat17 :: AddP "1" "7" "8"
-- else instance sumNat18 :: AddP "1" "8" "9"
-- else instance sumNat19 :: AddP "1" "9" "10"
-- else instance sumNat20 :: AddP "2" "0" "2"
-- else instance sumNat21 :: AddP "2" "1" "3"
-- else instance sumNat22 :: AddP "2" "2" "4"
-- else instance sumNat23 :: AddP "2" "3" "5"
-- else instance sumNat24 :: AddP "2" "4" "6"
-- else instance sumNat25 :: AddP "2" "5" "7"
-- else instance sumNat26 :: AddP "2" "6" "8"
-- else instance sumNat27 :: AddP "2" "7" "9"
-- else instance sumNat28 :: AddP "2" "8" "10"
-- else instance sumNat29 :: AddP "2" "9" "11"
-- else instance sumNat30 :: AddP "3" "0" "3"
-- else instance sumNat31 :: AddP "3" "1" "4"
-- else instance sumNat32 :: AddP "3" "2" "5"
-- else instance sumNat33 :: AddP "3" "3" "6"
-- else instance sumNat34 :: AddP "3" "4" "7"
-- else instance sumNat35 :: AddP "3" "5" "8"
-- else instance sumNat36 :: AddP "3" "6" "9"
-- else instance sumNat37 :: AddP "3" "7" "10"
-- else instance sumNat38 :: AddP "3" "8" "11"
-- else instance sumNat39 :: AddP "3" "9" "12"
-- else instance sumNat40 :: AddP "4" "0" "4"
-- else instance sumNat41 :: AddP "4" "1" "5"
-- else instance sumNat42 :: AddP "4" "2" "6"
-- else instance sumNat43 :: AddP "4" "3" "7"
-- else instance sumNat44 :: AddP "4" "4" "8"
-- else instance sumNat45 :: AddP "4" "5" "9"
-- else instance sumNat46 :: AddP "4" "6" "10"
-- else instance sumNat47 :: AddP "4" "7" "11"
-- else instance sumNat48 :: AddP "4" "8" "12"
-- else instance sumNat49 :: AddP "4" "9" "13"
-- else instance sumNat50 :: AddP "5" "0" "5"
-- else instance sumNat51 :: AddP "5" "1" "6"
-- else instance sumNat52 :: AddP "5" "2" "7"
-- else instance sumNat53 :: AddP "5" "3" "8"
-- else instance sumNat54 :: AddP "5" "4" "9"
-- else instance sumNat55 :: AddP "5" "5" "10"
-- else instance sumNat56 :: AddP "5" "6" "11"
-- else instance sumNat57 :: AddP "5" "7" "12"
-- else instance sumNat58 :: AddP "5" "8" "13"
-- else instance sumNat59 :: AddP "5" "9" "14"
-- else instance sumNat60 :: AddP "6" "0" "6"
-- else instance sumNat61 :: AddP "6" "1" "7"
-- else instance sumNat62 :: AddP "6" "2" "8"
-- else instance sumNat63 :: AddP "6" "3" "9"
-- else instance sumNat64 :: AddP "6" "4" "10"
-- else instance sumNat65 :: AddP "6" "5" "11"
-- else instance sumNat66 :: AddP "6" "6" "12"
-- else instance sumNat67 :: AddP "6" "7" "13"
-- else instance sumNat68 :: AddP "6" "8" "14"
-- else instance sumNat69 :: AddP "6" "9" "15"
-- else instance sumNat70 :: AddP "7" "0" "7"
-- else instance sumNat71 :: AddP "7" "1" "8"
-- else instance sumNat72 :: AddP "7" "2" "9"
-- else instance sumNat73 :: AddP "7" "3" "10"
-- else instance sumNat74 :: AddP "7" "4" "11"
-- else instance sumNat75 :: AddP "7" "5" "12"
-- else instance sumNat76 :: AddP "7" "6" "13"
-- else instance sumNat77 :: AddP "7" "7" "14"
-- else instance sumNat78 :: AddP "7" "8" "15"
-- else instance sumNat79 :: AddP "7" "9" "16"
-- else instance sumNat80 :: AddP "8" "0" "8"
-- else instance sumNat81 :: AddP "8" "1" "9"
-- else instance sumNat82 :: AddP "8" "2" "10"
-- else instance sumNat83 :: AddP "8" "3" "11"
-- else instance sumNat84 :: AddP "8" "4" "12"
-- else instance sumNat85 :: AddP "8" "5" "13"
-- else instance sumNat86 :: AddP "8" "6" "14"
-- else instance sumNat87 :: AddP "8" "7" "15"
-- else instance sumNat88 :: AddP "8" "8" "16"
-- else instance sumNat89 :: AddP "8" "9" "17"
-- else instance sumNat90 :: AddP "9" "0" "9"
-- else instance sumNat91 :: AddP "9" "1" "10"
-- else instance sumNat92 :: AddP "9" "2" "11"
-- else instance sumNat93 :: AddP "9" "3" "12"
-- else instance sumNat94 :: AddP "9" "4" "13"
-- else instance sumNat95 :: AddP "9" "5" "14"
-- else instance sumNat96 :: AddP "9" "6" "15"
-- else instance sumNat97 :: AddP "9" "7" "16"
-- else instance sumNat98 :: AddP "9" "8" "17"
-- else instance sumNat99 :: AddP "9" "9" "18"

instance addPD0ToNat :: AddP "0" y y
else instance addPD1ToSucc :: Succ y z => AddP "1" y z
else instance addPD2ToSucc :: (Succ z z', AddP "1" y z) => AddP "2" y z'
else instance addPD3ToSucc :: (Succ z z', AddP "2" y z) => AddP "3" y z'
else instance addPD4ToSucc :: (Succ z z', AddP "3" y z) => AddP "4" y z'
else instance addPD5ToSucc :: (Succ z z', AddP "4" y z) => AddP "5" y z'
else instance addPD6ToSucc :: (Succ z z', AddP "5" y z) => AddP "6" y z'
else instance addPD7ToSucc :: (Succ z z', AddP "6" y z) => AddP "7" y z'
else instance addPD8ToSucc :: (Succ z z', AddP "7" y z) => AddP "8" y z'
else instance addPD9ToSucc :: (Succ z z', AddP "8" y z) => AddP "9" y z'
-- else instance addSucc :: (IsNat a', Succ a a', Succ c c', AddP a b c) => AddP a' b c'
else instance addPMultiDigits :: (IsPos x, IsNat x, DivMod10 x xi xl, DivMod10 y yi yl, AddP xi yi zi, Append zi yl ziyl, Append xi xl xixl, AddP xl ziyl z) => AddP x y z


class (AddP x y z, AddP y x z) <= Add x y z | x y -> z, z x -> y, z y -> x
instance addTypeLevelRelation :: (AddP x y z, AddP y x z) => Add x y z

addT :: forall x y z. (Add x y z) => NProxy x -> NProxy y -> NProxy z
addT _ _ = undef

class Sub (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z, z x -> y, z y -> x
instance subtractTypeLevelRelation :: Add x y z => Sub z y x

subT :: forall x y z. (Sub x y z) => NProxy x -> NProxy y -> NProxy z
subT _ _ = undef

-- Ordering

class NatOrder (a :: Symbol) (b :: Symbol) (ord :: Ordering) | a b -> ord

instance orderSame :: NatOrder x x EQ
else instance orderEmpty :: NatOrder "" x LT
else instance orderEmpty' :: NatOrder x "" GT

-- putStrLn $ unlines $ map (\(a, b, c) -> show a ++ " " ++ show b ++" " ++ show c) $ map (\(a,b) -> (a, b, compare a b)) $ [(a, b) :: (Int, Int) | a <- [0..9], b <- [0..9]]
else instance order_0_0 :: NatOrder "0" "0" EQ
else instance order_0_1 :: NatOrder "0" "1" LT
else instance order_0_2 :: NatOrder "0" "2" LT
else instance order_0_3 :: NatOrder "0" "3" LT
else instance order_0_4 :: NatOrder "0" "4" LT
else instance order_0_5 :: NatOrder "0" "5" LT
else instance order_0_6 :: NatOrder "0" "6" LT
else instance order_0_7 :: NatOrder "0" "7" LT
else instance order_0_8 :: NatOrder "0" "8" LT
else instance order_0_9 :: NatOrder "0" "9" LT
else instance order_1_0 :: NatOrder "1" "0" GT
else instance order_1_1 :: NatOrder "1" "1" EQ
else instance order_1_2 :: NatOrder "1" "2" LT
else instance order_1_3 :: NatOrder "1" "3" LT
else instance order_1_4 :: NatOrder "1" "4" LT
else instance order_1_5 :: NatOrder "1" "5" LT
else instance order_1_6 :: NatOrder "1" "6" LT
else instance order_1_7 :: NatOrder "1" "7" LT
else instance order_1_8 :: NatOrder "1" "8" LT
else instance order_1_9 :: NatOrder "1" "9" LT
else instance order_2_0 :: NatOrder "2" "0" GT
else instance order_2_1 :: NatOrder "2" "1" GT
else instance order_2_2 :: NatOrder "2" "2" EQ
else instance order_2_3 :: NatOrder "2" "3" LT
else instance order_2_4 :: NatOrder "2" "4" LT
else instance order_2_5 :: NatOrder "2" "5" LT
else instance order_2_6 :: NatOrder "2" "6" LT
else instance order_2_7 :: NatOrder "2" "7" LT
else instance order_2_8 :: NatOrder "2" "8" LT
else instance order_2_9 :: NatOrder "2" "9" LT
else instance order_3_0 :: NatOrder "3" "0" GT
else instance order_3_1 :: NatOrder "3" "1" GT
else instance order_3_2 :: NatOrder "3" "2" GT
else instance order_3_3 :: NatOrder "3" "3" EQ
else instance order_3_4 :: NatOrder "3" "4" LT
else instance order_3_5 :: NatOrder "3" "5" LT
else instance order_3_6 :: NatOrder "3" "6" LT
else instance order_3_7 :: NatOrder "3" "7" LT
else instance order_3_8 :: NatOrder "3" "8" LT
else instance order_3_9 :: NatOrder "3" "9" LT
else instance order_4_0 :: NatOrder "4" "0" GT
else instance order_4_1 :: NatOrder "4" "1" GT
else instance order_4_2 :: NatOrder "4" "2" GT
else instance order_4_3 :: NatOrder "4" "3" GT
else instance order_4_4 :: NatOrder "4" "4" EQ
else instance order_4_5 :: NatOrder "4" "5" LT
else instance order_4_6 :: NatOrder "4" "6" LT
else instance order_4_7 :: NatOrder "4" "7" LT
else instance order_4_8 :: NatOrder "4" "8" LT
else instance order_4_9 :: NatOrder "4" "9" LT
else instance order_5_0 :: NatOrder "5" "0" GT
else instance order_5_1 :: NatOrder "5" "1" GT
else instance order_5_2 :: NatOrder "5" "2" GT
else instance order_5_3 :: NatOrder "5" "3" GT
else instance order_5_4 :: NatOrder "5" "4" GT
else instance order_5_5 :: NatOrder "5" "5" EQ
else instance order_5_6 :: NatOrder "5" "6" LT
else instance order_5_7 :: NatOrder "5" "7" LT
else instance order_5_8 :: NatOrder "5" "8" LT
else instance order_5_9 :: NatOrder "5" "9" LT
else instance order_6_0 :: NatOrder "6" "0" GT
else instance order_6_1 :: NatOrder "6" "1" GT
else instance order_6_2 :: NatOrder "6" "2" GT
else instance order_6_3 :: NatOrder "6" "3" GT
else instance order_6_4 :: NatOrder "6" "4" GT
else instance order_6_5 :: NatOrder "6" "5" GT
else instance order_6_6 :: NatOrder "6" "6" EQ
else instance order_6_7 :: NatOrder "6" "7" LT
else instance order_6_8 :: NatOrder "6" "8" LT
else instance order_6_9 :: NatOrder "6" "9" LT
else instance order_7_0 :: NatOrder "7" "0" GT
else instance order_7_1 :: NatOrder "7" "1" GT
else instance order_7_2 :: NatOrder "7" "2" GT
else instance order_7_3 :: NatOrder "7" "3" GT
else instance order_7_4 :: NatOrder "7" "4" GT
else instance order_7_5 :: NatOrder "7" "5" GT
else instance order_7_6 :: NatOrder "7" "6" GT
else instance order_7_7 :: NatOrder "7" "7" EQ
else instance order_7_8 :: NatOrder "7" "8" LT
else instance order_7_9 :: NatOrder "7" "9" LT
else instance order_8_0 :: NatOrder "8" "0" GT
else instance order_8_1 :: NatOrder "8" "1" GT
else instance order_8_2 :: NatOrder "8" "2" GT
else instance order_8_3 :: NatOrder "8" "3" GT
else instance order_8_4 :: NatOrder "8" "4" GT
else instance order_8_5 :: NatOrder "8" "5" GT
else instance order_8_6 :: NatOrder "8" "6" GT
else instance order_8_7 :: NatOrder "8" "7" GT
else instance order_8_8 :: NatOrder "8" "8" EQ
else instance order_8_9 :: NatOrder "8" "9" LT
else instance order_9_0 :: NatOrder "9" "0" GT
else instance order_9_1 :: NatOrder "9" "1" GT
else instance order_9_2 :: NatOrder "9" "2" GT
else instance order_9_3 :: NatOrder "9" "3" GT
else instance order_9_4 :: NatOrder "9" "4" GT
else instance order_9_5 :: NatOrder "9" "5" GT
else instance order_9_6 :: NatOrder "9" "6" GT
else instance order_9_7 :: NatOrder "9" "7" GT
else instance order_9_8 :: NatOrder "9" "8" GT
else instance order_9_9 :: NatOrder "9" "9" EQ
else instance order_n_n :: (Snoc xh xl x, Snoc yh yl y, NatOrder xh yh orderh, NatOrder xl yl orderl, O.Append orderh orderl order) => NatOrder x y order

order :: ∀a b r. NatOrder a b r => NProxy a -> NProxy b -> OProxy r
order _ = undef