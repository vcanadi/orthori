{-# LANGUAGE DeriveFunctor #-}

module Orth.P3 where

{- Low-level 3d vector type
-}

import GHC.Generics (Generic)
import Orth.Sy

-- | Simple 3d point
data P3 n = P3 n n n
  deriving (Show, Eq, Read, Generic, Functor)

instance Num n => Semigroup (P3 n) where (<>) = p3Add
instance Num n => Monoid (P3 n) where mempty = P3 0 0 0

-- | Scalar multiplication
p3Scale :: Num n => n -> P3 n -> P3 n
p3Scale a (P3 x y z) = P3 (a*x) (a*y) (a*z)

p3Add :: Num n => P3 n -> P3 n -> P3 n
P3 x0 y0 z0 `p3Add` P3 x1 y1 z1 = P3 (x0 + x1) (y0 + y1) (z0 + z1)

p3Sub :: Num n => P3 n -> P3 n -> P3 n
P3 x0 y0 z0 `p3Sub` P3 x1 y1 z1 = P3 (x0 - x1) (y0 - y1) (z0 - z1)

p3Dot :: Num n => P3 n -> P3 n -> n
P3 x y z `p3Dot` P3 x' y' z' = x*x' + y*y' + z*z'

p3Cross :: Num n => P3 n -> P3 n -> P3 n
P3 x y z `p3Cross` P3 x' y' z' = P3 (y*z' - z*y') (z*x' - x*z') (x*y' - y*x')

type P3Sy = P3 Sy

evalP3 :: [(String, Int)] -> P3 Sy -> P3 Int
evalP3 vs (P3 x y z) = P3 (evalSy vs x) (evalSy vs y) (evalSy vs z)
