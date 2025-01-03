{-# LANGUAGE LambdaCase #-}

module Orth.Sy where

{- Simple symbolic language for describing parameterized measurements
-}

import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)

-- | Symbolic value
data Sy = Vr String -- ^ Named variable
        | C Int -- ^ Constant
        | Neg Sy
        | Add Sy Sy
        | Mul Sy Sy
        | Max Sy Sy
        | Min Sy Sy
        | Sig Sy
        | Abs Sy
        deriving (Show, Eq, Read, Generic)

-- | Num instance with some quick simplification
instance Num Sy where
  C x + C y = C $ x + y
  (C 0) + y = y
  x + (C 0) = x
  x + y = Add x y
  negate (C 0) = C 0
  negate (C x) = C $ negate x
  negate (Add x y) = negate x - y
  negate x = Neg x
  C x * C y = C $ x * y
  (C 0) * _ = 0
  _ * (C 0) = 0
  (C 1) * y = y
  x * (C 1) = x
  (C (-1)) * y = Neg y
  x * (C (-1)) = Neg x
  x * y = Mul x y
  abs (Abs x) = Abs x
  abs (Mul x y) = abs x * abs y
  abs x = Abs x
  fromInteger x = C $ fromInteger x
  signum (Sig x) = Sig x
  signum (Mul x y) = signum x * signum y
  signum x = Sig x

evalSy :: [(String, Int)] -> Sy -> Int
evalSy vs = \case
  Vr nm -> fromMaybe 0 $ lookup nm vs
  C c ->  c
  Neg sy -> negate (evalSy vs sy)
  Add sy0 sy1 -> evalSy vs sy0 + evalSy vs sy1
  Mul sy0 sy1 -> evalSy vs sy0 * evalSy vs sy1
  Max sy0 sy1 -> max (evalSy vs sy0) (evalSy vs sy1)
  Min sy0 sy1 -> min (evalSy vs sy0) (evalSy vs sy1)
  Sig sy -> signum (evalSy vs sy)
  Abs sy -> abs (evalSy vs sy)

syShow :: Sy -> String
syShow (Vr n) = n
syShow (C x) = show x
syShow (Neg x) = "-" <> syShow x
syShow (Add  x y) = syShow x <> " + " <> syShow y
syShow (Mul  x y) = syShow x <> " * " <> syShow y
