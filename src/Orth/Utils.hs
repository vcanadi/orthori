{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Orth.Utils where

{- Helper functions
-}

import Control.Arrow ((>>>))
import Data.Proxy (Proxy (Proxy))

-- | Modular binary function on enum type
enumBinF :: forall a. (Enum a, Bounded a) => (Int -> Int -> Int) -> a -> a -> a
{-# INLINE enumBinF #-}
enumBinF g x y = toEnum $ g (fromEnum x) (fromEnum y) `mod` enumLen (Proxy @a)

-- | Number of enum values for some type
enumLen :: forall a. (Enum a, Bounded a) => Proxy a -> Int
enumLen _ = succ $ fromEnum $ maxBound @a

-- | Modular addtion on enum type
enumAdd :: forall a. (Enum a, Bounded a) => a -> a -> a
enumAdd = enumBinF (+)

-- | Modular multiplication on enum type
enumMul :: forall a. (Enum a, Bounded a) => a -> a -> a
enumMul = enumBinF (*)

-- | Type value corresponding to enum 0
enumZero :: Enum a => a
enumZero = toEnum 0

-- | Modular multiplication on enum type
enumNegate :: forall a. (Enum a, Bounded a) => a -> a
enumNegate x = toEnum $ enumLen (Proxy @a) - fromEnum x

-- | Modular multiplication on enum type
enumSignum :: forall a. Enum a => a -> a
enumSignum = fromEnum >>> \case
  0 -> toEnum 0
  x -> toEnum x

enumAll :: (Enum a, Bounded a) => [a]
enumAll = [minBound..maxBound]
