module Orth.Quaternion where

import GHC.Generics (Generic)
import Orth.P3

{- Simple quaternion implementation for represendint any 3d rotation
-}

data Quat n = Quat { quatS :: n, quatV :: P3 n } deriving (Show, Eq, Generic)

qMul :: Num n => Quat n -> Quat n -> Quat n
Quat r1 v1  `qMul` Quat r2 v2 = Quat (r1*r2 - v1 `p3Dot` v2) ((r1 `p3Scale` v2) `p3Add` (r2 `p3Scale` v1)  `p3Add` (v1 `p3Cross` v2))

qScale :: Num n => n -> Quat n -> Quat n
qScale a (Quat r v) = Quat (a*r) $ a `p3Scale` v

qInv :: Fractional n => Quat n -> Quat n
qInv (Quat r v) = Quat (r/n) $ p3Scale (-n) v
  where
    n = r*r + v `p3Dot` v

p3ToQuat :: Num n => P3 n -> Quat n
p3ToQuat = Quat 0

quatMulP3 :: Num n => Quat n -> P3 n -> P3 n
quatMulP3 q p = quatV $ q `qMul` p3ToQuat p

p3MulQuat :: Num n => P3 n -> Quat n -> P3 n
p3MulQuat p q = quatV $ p3ToQuat p `qMul` q

-- | Rotation around axis
axisAngle :: Floating n => P3 n -> n -> Quat n
axisAngle ax ang = Quat (cos $ ang/2) $ sin (ang/2) `p3Scale` ax

-- | Inverse of rotation around axis
axisAngleInv :: Floating n => P3 n -> n -> Quat n
axisAngleInv ax ang = Quat (cos $ ang/2) $ (-sin (ang/2)) `p3Scale` ax

-- | Apply rotation around some axis
rotateAround :: Floating n => P3 n -> n -> P3 n -> P3 n
rotateAround ax ang v = quatV $ (axisAngle ax ang `qMul` p3ToQuat v) `qMul` axisAngleInv ax ang

