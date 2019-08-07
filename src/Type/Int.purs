module Type.Int where

import Nat
import Prelude

import Data.Symbol (SProxy(..))
import Prim.Symbol (class Cons)
import Type.Data.Boolean (class If)
import Type.Prelude (kind Boolean, True, False)

-- foreign import data IProxy :: Symbol -> Type

-- class IsInt (a :: Symbol) where
--   reflectInt :: IProxy a -> Int

-- class IsSign' (a :: Symbol) (isSign :: Boolean) | a -> isSign, isSign -> a where
--   reflectSign :: SProxy a -> Int

-- instance negSign' :: IsSign' "-" True where
--   reflectSign _ = -1
-- else
-- instance negElse :: IsSign' a False where
--   reflectSign _ = 1

-- instance posIsInt :: (Cons head tail sym, IsSign' head isHeadSign, If isHeadSign (SProxy tail) (SProxy sym) (SProxy nat), IsNat nat) => IsInt sym where
--   reflectInt _ = reflectSign (undef :: SProxy head) * reflectNat (undef :: NProxy nat)

-- testReflectInt1 :: Int
-- testReflectInt1 = reflectInt (undef :: IProxy "12")

-- testReflectInt2 :: Int
-- testReflectInt2 = reflectInt (undef :: IProxy "-12")

