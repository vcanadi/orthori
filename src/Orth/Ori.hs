{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}

module Orth.Ori where

{- Discrete orthogonal orientations in 3d
-}

import GHC.Generics (Generic)

import Orth.Utils
import Data.Bool (bool)
import Orth.P3 (P3 (P3))

-- | Base two digits with modular artithmetic (mod 2) via Num instance
data B2 = B20 | B21 deriving (Show, Eq, Read, Enum, Bounded, Generic)

instance Num B2 where (+) = enumAdd; (*) = enumMul; abs = id; signum = enumSignum; negate = enumNegate; fromInteger = toEnum . fromIntegral

-- | Boolean negation
ne :: B2 -> B2
ne B20 = B21
ne B21 = B20

-- | Base three digits with modular artithmetic (mod 3) via Num instance
data B3 = B30 | B31 | B32 deriving (Show, Eq, Read, Enum, Bounded, Generic)

instance Num B3 where (+) = enumAdd; (*) = enumMul; abs = id; signum = enumSignum; negate = enumNegate; fromInteger = toEnum . fromIntegral

-- | Lift B2 into B3
b2ToB3 :: B2 -> B3
b2ToB3 = toEnum . fromEnum

-- | Orientation plane (bivector Ori invariant to 90 rotations, and Turn between forward and up)
newtype Plane = Plane B3
  deriving (Show, Eq, Read, Enum, Bounded, Num, Generic)

isB30 :: B3 -> B2
isB30 B30 = B21
isB30 _   = B20

isB31 :: B3 -> B2
isB31 B31 = B21
isB31 _   = B20

isB32 :: B3 -> B2
isB32 B32 = B21
isB32 _   = B20

-- | Rotation direction e.g. in XZ plane from x to z or z to x
newtype Turn = Turn B2 deriving (Show, Eq, Read, Enum, Bounded, Num, Generic)

-- | Direction in which forward vector is pointing
newtype Forw = Forw B2 deriving (Show, Eq, Read, Enum, Bounded, Num, Generic)

-- | Direction in which up vector is pointing
newtype Up = Up B2 deriving (Show, Eq, Read, Enum, Bounded, Generic)

-- | Factorized Orientation for easier calculation (Plane (3) * Turn (2) * Forward(2) * Up (2) = Ori (24) )
data Ori = Ori { foP :: Plane, foT :: Turn, foF :: Forw, foU :: Up } deriving (Show, Eq, Read, Generic)

pattern OFU, OUB, ODF, OBD, OUF, OFD, OBU, ODB, OLF, OFR, OBL
      , ORB, OFL, OLB, ORF, OBR, OUL, OLD, ORU, ODR, OLU, OUR, ODL, ORD :: Ori
pattern OFU = Ori (Plane B30) (Turn B20) (Forw B20) (Up B20)
pattern OUB = Ori (Plane B30) (Turn B20) (Forw B20) (Up B21)
pattern ODF = Ori (Plane B30) (Turn B20) (Forw B21) (Up B20)
pattern OBD = Ori (Plane B30) (Turn B20) (Forw B21) (Up B21)

pattern OUF = Ori (Plane B30) (Turn B21) (Forw B20) (Up B20)
pattern OFD = Ori (Plane B30) (Turn B21) (Forw B20) (Up B21)
pattern OBU = Ori (Plane B30) (Turn B21) (Forw B21) (Up B20)
pattern ODB = Ori (Plane B30) (Turn B21) (Forw B21) (Up B21)

pattern OLF = Ori (Plane B31) (Turn B20) (Forw B20) (Up B20)
pattern OFR = Ori (Plane B31) (Turn B20) (Forw B20) (Up B21)
pattern OBL = Ori (Plane B31) (Turn B20) (Forw B21) (Up B20)
pattern ORB = Ori (Plane B31) (Turn B20) (Forw B21) (Up B21)

pattern OFL = Ori (Plane B31) (Turn B21) (Forw B20) (Up B20)
pattern OLB = Ori (Plane B31) (Turn B21) (Forw B20) (Up B21)
pattern ORF = Ori (Plane B31) (Turn B21) (Forw B21) (Up B20)
pattern OBR = Ori (Plane B31) (Turn B21) (Forw B21) (Up B21)

pattern OUL = Ori (Plane B32) (Turn B20) (Forw B20) (Up B20)
pattern OLD = Ori (Plane B32) (Turn B20) (Forw B20) (Up B21)
pattern ORU = Ori (Plane B32) (Turn B20) (Forw B21) (Up B20)
pattern ODR = Ori (Plane B32) (Turn B20) (Forw B21) (Up B21)

pattern OLU = Ori (Plane B32) (Turn B21) (Forw B20) (Up B20)
pattern OUR = Ori (Plane B32) (Turn B21) (Forw B20) (Up B21)
pattern ODL = Ori (Plane B32) (Turn B21) (Forw B21) (Up B20)
pattern ORD = Ori (Plane B32) (Turn B21) (Forw B21) (Up B21)

-- | List of all orientations
allOris :: [Ori]
allOris = [Ori p t f u | p <- enumAll, t <- enumAll, f <- enumAll, u <- enumAll ]

-- Composition of two orientations i.e. second orientation relative to the first
-- Full multiplication table
--            FU   UB     DF   BD       UF   FD     BU   DB         LF   FR     BL   RB       FL   LB     RF   BR         UL   LD     RU   DR       LU   UR     DL   RD
--            ptfu
--         B  0000 0001   0010 0011     0100 0101   0110 0111       1000 1001   1010 1011     1100 1101   1110 1111       2000 2001   2010 2011     2100 2101   2110 2111
-- A
--    ptfu
-- FU 0000    0000 0001   0010 0011     0100 0101   0110 0111       1000 1001   1010 1011     1100 1101   1110 1111       2000 2001   2010 2011     2100 2101   2110 2111
-- UB 0001    0001 0011   0000 0010     0110 0100   0111 0101       2100 2101   2110 2111     2000 2001   2010 2011       1010 1000   1011 1001     1101 1111   1100 1110
--
-- DF 0010    0010 0000   0011 0001     0101 0111   0100 0110       2001 2011   2000 2010     2110 2100   2111 2101       1100 1101   1110 1111     1000 1001   1010 1011
-- BD 0011    0011 0010   0001 0000     0111 0110   0101 0100       1101 1111   1100 1110     1010 1000   1011 1001       2110 2100   2111 2101     2001 2011   2000 2010
--
--
-- UF 0100    0100 0101   0110 0111     0000 0001   0010 0011       2010 2000   2011 2001     2101 2111   2100 2110       1001 1011   1000 1010     1110 1100   1111 1101
-- FD 0101    0101 0111   0100 0110     0010 0000   0011 0001       1110 1100   1111 1101     1001 1011   1000 1010       2011 2010   2001 2000     2111 2110   2101 2100
--
-- BU 0110    0110 0100   0111 0101     0001 0011   0000 0010       1011 1010   1001 1000     1111 1110   1101 1100       2101 2111   2100 2110     2010 2000   2011 2001
-- DB 0111    0111 0110   0101 0100     0011 0010   0001 0000       2111 2110   2101 2100     2011 2010   2001 2000       1111 1110   1101 1100     1011 1010   1001 1000
--
--
--
-- LF 1000    1000 1001   1010 1011     1100 1101   1110 1111       2000 2001   2010 2011     2100 2101   2110 2111       0000 0001   0010 0011     0100 0101   0110 0111
-- FR 1001    1001 1011   1000 1010     1110 1100   1111 1101       0100 0101   0110 0111     0000 0001   0010 0011       2010 2000   2011 2001     2101 2111   2100 2110
--
-- BL 1010    1010 1000   1011 1001     1101 1111   1100 1110       0001 0011   0000 0010     0110 0100   0111 0101       2100 2101   2110 2111     2000 2001   2010 2011
-- RB 1011    1011 1010   1001 1000     1111 1110   1101 1100       2101 2111   2100 2110     2010 2000   2011 2001       0110 0100   0111 0101     0001 0011   0000 0010
--
--
-- FL 1100    1100 1101   1110 1111     1000 1001   1010 1011       0010 0000   0011 0001     0101 0111   0100 0110       2001 2011   2000 2010     2110 2100   2111 2101
-- LB 1101    1101 1111   1100 1110     1010 1000   1011 1001       2110 2100   2111 2101     2001 2011   2000 2010       0011 0010   0001 0000     0111 0110   0101 0100
--
-- RF 1110    1110 1100   1111 1101     1001 1011   1000 1010       2011 2010   2001 2000     2111 2110   2101 2100       0101 0111   0100 0110     0010 0000   0011 0001
-- BR 1111    1111 1110   1101 1100     1011 1010   1001 1000       0111 0110   0101 0100     0011 0010   0001 0000       2111 2110   2101 2100     2011 2010   2001 2000
--
--
--
-- UL 2000    2000 2001   2010 2011     2100 2101   2110 2111       0000 0001   0010 0011     0100 0101   0110 0111       1000 1001   1010 1011     1100 1101   1110 1111
-- LD 2001    2001 2011   2000 2010     2110 2100   2111 2101       1100 1101   1110 1111     1000 1001   1010 1011       0010 0000   0011 0001     0101 0111   0100 0110
--
-- RU 2010    2010 2000   2011 2001     2101 2111   2100 2110       1001 1011   1000 1010     1110 1100   1111 1101       0100 0101   0110 0111     0000 0001   0010 0011
-- DR 2011    2011 2010   2001 2000     2111 2110   2101 2100       0101 0111   0100 0110     0010 0000   0011 0001       1110 1100   1111 1101     1001 1011   1000 1010
--
--
-- LU 2100    2100 2101   2110 2111     2000 2001   2010 2011       1010 1000   1011 1001     1101 1111   1100 1110       0001 0011   0000 0010     0110 0100   0111 0101
-- UR 2101    2101 2111   2100 2110     2010 2000   2011 2001       0110 0100   0111 0101     0001 0011   0000 0010       1011 1010   1001 1000     1111 1110   1101 1100
--
-- DL 2110    2110 2100   2111 2101     2001 2011   2000 2010       0011 0010   0001 0000     0111 0110   0101 0100       1101 1111   1100 1110     1010 1000   1011 1001
-- RD 2111    2111 2110   2101 2100     2011 2010   2001 2000       1111 1110   1101 1100     1011 1010   1001 1000       0111 0110   0101 0100     0011 0010   0001 0000
--
-- algebraic function describing the table
oriMul :: Ori -> Ori -> Ori
oriMul a b = Ori (Plane $ pMul a b) (Turn $ tMul a b) (Forw $ fMul a b) (Up $ uMul a b)

-- P component of ori multiplication function
--            FU   UB     DF   BD       UF   FD     BU   DB         LF   FR     BL   RB       FL   LB     RF   BR         UL   LD     RU   DR       LU   UR     DL   RD
--            ptfu
--         B  0000 0001   0010 0011     0100 0101   0110 0111       1000 1001   1010 1011     1100 1101   1110 1111       2000 2001   2010 2011     2100 2101   2110 2111
-- A
--    ptfu
-- FU 0000    0    0      0    0        0    0      0    0          1    1      1    1        1    1      1    1          2    2      2    2        2    2      2    2
-- UB 0001    0    0      0    0        0    0      0    0          2    2      2    2        2    2      2    2          1    1      1    1        1    1      1    1
--
-- DF 0010    0    0      0    0        0    0      0    0          2    2      2    2        2    2      2    2          1    1      1    1        1    1      1    1
-- BD 0011    0    0      0    0        0    0      0    0          1    1      1    1        1    1      1    1          2    2      2    2        2    2      2    2
--
--
-- UF 0100    0    0      0    0        0    0      0    0          2    2      2    2        2    2      2    2          1    1      1    1        1    1      1    1
-- FD 0101    0    0      0    0        0    0      0    0          1    1      1    1        1    1      1    1          2    2      2    2        2    2      2    2
--
-- BU 0110    0    0      0    0        0    0      0    0          1    1      1    1        1    1      1    1          2    2      2    2        2    2      2    2
-- DB 0111    0    0      0    0        0    0      0    0          2    2      2    2        2    2      2    2          1    1      1    1        1    1      1    1
--
--
--
-- LF 1000    1    1      1    1        1    1      1    1          2    2      2    2        2    2      2    2          0    0      0    0        0    0      0    0
-- FR 1001    1    1      1    1        1    1      1    1          0    0      0    0        0    0      0    0          2    2      2    2        2    2      2    2
--
-- BL 1010    1    1      1    1        1    1      1    1          0    0      0    0        0    0      0    0          2    2      2    2        2    2      2    2
-- RB 1011    1    1      1    1        1    1      1    1          2    2      2    2        2    2      2    2          0    0      0    0        0    0      0    0
--
--
-- FL 1100    1    1      1    1        1    1      1    1          0    0      0    0        0    0      0    0          2    2      2    2        2    2      2    2
-- LB 1101    1    1      1    1        1    1      1    1          2    2      2    2        2    2      2    2          0    0      0    0        0    0      0    0
--
-- RF 1110    1    1      1    1        1    1      1    1          2    2      2    2        2    2      2    2          0    0      0    0        0    0      0    0
-- BR 1111    1    1      1    1        1    1      1    1          0    0      0    0        0    0      0    0          2    2      2    2        2    2      2    2
--
--
--
-- UL 2000    2    2      2    2        2    2      2    2          0    0      0    0        0    0      0    0          1    1      1    1        1    1      1    1
-- LD 2001    2    2      2    2        2    2      2    2          1    1      1    1        1    1      1    1          0    0      0    0        0    0      0    0
--
-- RU 2010    2    2      2    2        2    2      2    2          1    1      1    1        1    1      1    1          0    0      0    0        0    0      0    0
-- DR 2011    2    2      2    2        2    2      2    2          0    0      0    0        0    0      0    0          1    1      1    1        1    1      1    1
--
--
-- LU 2100    2    2      2    2        2    2      2    2          1    1      1    1        1    1      1    1          0    0      0    0        0    0      0    0
-- UR 2101    2    2      2    2        2    2      2    2          0    0      0    0        0    0      0    0          1    1      1    1        1    1      1    1
--
-- DL 2110    2    2      2    2        2    2      2    2          0    0      0    0        0    0      0    0          1    1      1    1        1    1      1    1
-- RD 2111    2    2      2    2        2    2      2    2          1    1      1    1        1    1      1    1          0    0      0    0        0    0      0    0
--
pMul :: Ori -> Ori -> B3
pMul (Ori (Plane pA) (Turn tA) (Forw fA) (Up uA)) (Ori (Plane pB) _ _ _)
  =  pA + pB + pB * b2ToB3 (tA + fA + uA)

-- T component of ori multiplication function
--            FU   UB     DF   BD       UF   FD     BU   DB         LF   FR     BL   RB       FL   LB     RF   BR         UL   LD     RU   DR       LU   UR     DL   RD
--            ptfu
--         B  0000 0001   0010 0011     0100 0101   0110 0111       1000 1001   1010 1011     1100 1101   1110 1111       2000 2001   2010 2011     2100 2101   2110 2111
-- A
--    ptfu
-- FU 0000     0    0      0    0        1    1      1    1          0    0      0    0        1    1      1    1          0    0      0    0        1    1      1    1
-- UB 0001     0    0      0    0        1    1      1    1          1    1      1    1        0    0      0    0          0    0      0    0        1    1      1    1
--
-- DF 0010     0    0      0    0        1    1      1    1          0    0      0    0        1    1      1    1          1    1      1    1        0    0      0    0
-- BD 0011     0    0      0    0        1    1      1    1          1    1      1    1        0    0      0    0          1    1      1    1        0    0      0    0
--
--
-- UF 0100     1    1      1    1        0    0      0    0          0    0      0    0        1    1      1    1          0    0      0    0        1    1      1    1
-- FD 0101     1    1      1    1        0    0      0    0          1    1      1    1        0    0      0    0          0    0      0    0        1    1      1    1
--
-- BU 0110     1    1      1    1        0    0      0    0          0    0      0    0        1    1      1    1          1    1      1    1        0    0      0    0
-- DB 0111     1    1      1    1        0    0      0    0          1    1      1    1        0    0      0    0          1    1      1    1        0    0      0    0
--                       ..... periodic with pA
--
tMul :: Ori -> Ori -> B2
tMul (Ori _ (Turn tA) (Forw fA) (Up uA)) (Ori (Plane pB) (Turn tB) _ _)
 = isB30 pB * (tB + tA)
 + isB31 pB * (tB + uA)
 + isB32 pB * (tB + fA)


-- F component of ori multiplication function
--            FU   UB     DF   BD       UF   FD     BU   DB         LF   FR     BL   RB       FL   LB     RF   BR         UL   LD     RU   DR       LU   UR     DL   RD
--            ptfu
--         B  0000 0001   0010 0011     0100 0101   0110 0111       1000 1001   1010 1011     1100 1101   1110 1111       2000 2001   2010 2011     2100 2101   2110 2111
-- A
--    ptfu
-- FU 0000      0    0      1    1        0    0      1    1          0    0      1    1        0    0      1    1          0    0      1    1        0    0      1    1
-- UB 0001      0    1      0    1        1    0      1    0          0    0      1    1        0    0      1    1          1    0      1    0        0    1      0    1
--
-- DF 0010      1    0      1    0        0    1      0    1          0    1      0    1        1    0      1    0          0    0      1    1        0    0      1    1
-- BD 0011      1    1      0    0        1    1      0    0          0    1      0    1        1    0      1    0          1    0      1    0        0    1      0    1
--
--
-- UF 0100      0    0      1    1        0    0      1    1          1    0      1    0        0    1      0    1          0    1      0    1        1    0      1    0
-- FD 0101      0    1      0    1        1    0      1    0          1    0      1    0        0    1      0    1          1    1      0    0        1    1      0    0
--
-- BU 0110      1    0      1    0        0    1      0    1          1    1      0    0        1    1      0    0          0    1      0    1        1    0      1    0
-- DB 0111      1    1      0    0        1    1      0    0          1    1      0    0        1    1      0    0          1    1      0    0        1    1      0    0
--                       ..... periodic with pA
--
fMul :: Ori -> Ori -> B2
fMul (Ori _ (Turn tA) (Forw fA) (Up uA)) (Ori (Plane pB) (Turn tB) (Forw fB) (Up uB))
  = isB30 pB *
      ( ne fA * ne uA * fB
      + ne fA *    uA * (tB +  uB)
      + fA * ne uA * (tB + ne uB)
      + fA *    uA * ne fB
      )
  + isB31 pB *
      ( ne tA * ne fA * fB
      + ne tA *    fA * (tB +  uB)
      + tA * ne fA * (tB + ne uB)
      + tA *    fA * ne fB
      )
  + isB32 pB *
      ( ne uA * ne tA * fB
      + ne uA *    tA * (tB +  uB)
      + uA * ne tA * (tB + ne uB)
      + uA *    tA * ne fB
      )

--U component of ori multiplication function
--            FU   UB     DF   BD       UF   FD     BU   DB         LF   FR     BL   RB       FL   LB     RF   BR         UL   LD     RU   DR       LU   UR     DL   RD
--            ptfu
--         B  0000 0001   0010 0011     0100 0101   0110 0111       1000 1001   1010 1011     1100 1101   1110 1111       2000 2001   2010 2011     2100 2101   2110 2111
-- A
--    ptfu
-- FU 0000       0    1      0    1        0    1      0    1          0    1      0    1        0    1      0    1          0    1      0    1        0    1      0    1
-- UB 0001       1    1      0    0        0    0      1    1          0    1      0    1        0    1      0    1          0    0      1    1        1    1      0    0
--
-- DF 0010       0    0      1    1        1    1      0    0          1    1      0    0        0    0      1    1          0    1      0    1        0    1      0    1
-- BD 0011       1    0      1    0        1    0      1    0          1    1      0    0        0    0      1    1          0    0      1    1        1    1      0    0
--
--
-- UF 0100       0    1      0    1        0    1      0    1          0    0      1    1        1    1      0    0          1    1      0    0        0    0      1    1
-- FD 0101       1    1      0    0        0    0      1    1          0    0      1    1        1    1      0    0          1    0      1    0        1    0      1    0

-- BU 0110       0    0      1    1        1    1      0    0          1    0      1    0        1    0      1    0          1    1      0    0        0    0      1    1
-- DB 0111       1    0      1    0        1    0      1    0          1    0      1    0        1    0      1    0          1    0      1    0        1    0      1    0
--                  ............. periodic with pA
--
uMul :: Ori -> Ori -> B2
uMul (Ori _ (Turn tA) (Forw fA) (Up uA)) (Ori (Plane pB) (Turn tB) (Forw fB) (Up uB))
  = isB30 pB *
      ( ne fA * ne uA * uB
      + ne fA *    uA * (tB + ne fB)
      + fA * ne uA * (tB +    fB)
      + fA *    uA * ne uB
      )
  + isB31 pB *
      ( ne tA * ne fA * uB
      + ne tA *    fA * (tB + ne fB)
      + tA * ne fA * (tB +    fB)
      + tA *    fA * ne uB
      )
  + isB32 pB *
      ( ne uA * ne tA * uB
      + ne uA *    tA * (tB + ne fB)
      + uA * ne tA * (tB +    fB)
      + uA *    tA * ne uB
      )

-- | Convert Ori to 3x3 matrix with 0, 1 and -1 entries
oriMatrix :: Num n => Ori -> [[n]]
oriMatrix (Ori (Plane p') (Turn t') (Forw f') (Up u')) = fmap (fmap fromIntegral)
    [ [ notEq 2 p * (1-2*f) * ((1 + eq 1 p + t + f + u) `mod` 2),     eq 2 p * (1-2*t),     notEq 2 p * (1-2*u) * ((eq 1 p + t + f + u) `mod` 2) ]
    , [ notEq 0 p * (1-2*f) * ((1 + eq 2 p + t + f + u) `mod` 2),     eq 0 p * (1-2*t),     notEq 0 p * (1-2*u) * ((eq 2 p + t + f + u) `mod` 2) ]
    , [ notEq 1 p * (1-2*f) * ((1 + eq 0 p + t + f + u) `mod` 2),     eq 1 p * (1-2*t),     notEq 1 p * (1-2*u) * ((eq 0 p + t + f + u) `mod` 2) ]
    ]
    where
      (p, t, f, u) = (fromEnum p', fromEnum t', fromEnum f', fromEnum u')

-- | For n,k in {0,1,2}, k /= n, Interpolate of point (n,1) and (k,0)
eq :: Int -> Int -> Int
-- eq 0 p = (p-1)*(p-2) `div` ((0-1)*(0-2))
-- eq 1 p = (p-0)*(p-2) `div` ((1-0)*(1-2))
-- eq 2 p = (p-0)*(p-1) `div` ((2-0)*(2-1))
eq x y = bool 0 1 $ x==y

notEq :: Int -> Int -> Int
notEq n p = 1 - eq n p

oriPlane :: Ori -> Plane
oriPlane = foP

-- | Based on length, width and depth build a vector in corresponding coordinate system defined by Ori rotation
-- Resulting vector P3 l' w' d' is generated by [[l'],[w'],[d']] = oriMatrix ori `matMul` [[l],[w],[d]]
oriCS :: Num n  => Ori -> n -> n -> n -> P3 n
oriCS (Ori (Plane p) (Turn t) (Forw f) (Up u)) l w d = case (p,t,f,u) of
  (B30, B20, B20, B20) -> P3   l    w    d
  (B30, B20, B20, B21) -> P3 (-d)   w    l
  (B30, B20, B21, B20) -> P3   d    w  (-l)
  (B30, B20, B21, B21) -> P3 (-l)   w  (-d)
  (B30, B21, B20, B20) -> P3   d  (-w)   l
  (B30, B21, B20, B21) -> P3   l  (-w) (-d)
  (B30, B21, B21, B20) -> P3 (-l) (-w)   d
  (B30, B21, B21, B21) -> P3 (-d) (-w) (-l)
  (B31, B20, B20, B20) -> P3   d    l    w
  (B31, B20, B20, B21) -> P3   l  (-d)   w
  (B31, B20, B21, B20) -> P3 (-l)   d    w
  (B31, B20, B21, B21) -> P3 (-d) (-l)   w
  (B31, B21, B20, B20) -> P3   l    d  (-w)
  (B31, B21, B20, B21) -> P3 (-d)   l  (-w)
  (B31, B21, B21, B20) -> P3   d  (-l) (-w)
  (B31, B21, B21, B21) -> P3 (-l) (-d) (-w)
  (B32, B20, B20, B20) -> P3   w    d    l
  (B32, B20, B20, B21) -> P3   w    l  (-d)
  (B32, B20, B21, B20) -> P3   w  (-l)   d
  (B32, B20, B21, B21) -> P3   w  (-d) (-l)
  (B32, B21, B20, B20) -> P3 (-w)   l    d
  (B32, B21, B20, B21) -> P3 (-w) (-d)   l
  (B32, B21, B21, B20) -> P3 (-w)   d  (-l)
  (B32, B21, B21, B21) -> P3 (-w) (-l) (-d)

-- | Represent a P3 point in Ori coordinate system
p3OriCS :: Num n => Ori -> P3 n -> P3 n
p3OriCS ori (P3 x y z) = oriCS ori x y z

-- data Pose n = Pose (P3 n) Ori

-- instance Num n => Semigroup (Pose n) where
--   Pose p0 ori0 <> Pose p1 ori1 = Pose (p0 <> p3FromOri ori0 p1) (ori0 <> ori1)

-- -- | Something that has coordinate system (position and orientation)
-- class HasPose a n where
--   pose :: a -> (P3 n, Ori)

