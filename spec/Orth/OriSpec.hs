module Orth.OriSpec where

import Data.List(transpose)

import Test.Hspec
import Control.Monad (forM_)

import Orth.Ori
import Orth.P3

-- | Manually defined ground truth of all ori to matrix cases for testing
gtOriMatrix :: Ori -> [[Integer]]
gtOriMatrix (Ori (Plane p) (Turn t) (Forw f) (Up u)) = case (p,t,f,u) of
    (B30, B20, B20, B20) -> [ [ 1, 0, 0], [ 0, 1, 0], [ 0, 0, 1]]
    (B30, B20, B20, B21) -> [ [ 0, 0,-1], [ 0, 1, 0], [ 1, 0, 0]]
    (B30, B20, B21, B20) -> [ [ 0, 0, 1], [ 0, 1, 0], [-1, 0, 0]]
    (B30, B20, B21, B21) -> [ [-1, 0, 0], [ 0, 1, 0], [ 0, 0,-1]]
    (B30, B21, B20, B20) -> [ [ 0, 0, 1], [ 0,-1, 0], [ 1, 0, 0]]
    (B30, B21, B20, B21) -> [ [ 1, 0, 0], [ 0,-1, 0], [ 0, 0,-1]]
    (B30, B21, B21, B20) -> [ [-1, 0, 0], [ 0,-1, 0], [ 0, 0, 1]]
    (B30, B21, B21, B21) -> [ [ 0, 0,-1], [ 0,-1, 0], [-1, 0, 0]]
    (B31, B20, B20, B20) -> [ [ 0, 0, 1], [ 1, 0, 0], [ 0, 1, 0]]
    (B31, B20, B20, B21) -> [ [ 1, 0, 0], [ 0, 0,-1], [ 0, 1, 0]]
    (B31, B20, B21, B20) -> [ [-1, 0, 0], [ 0, 0, 1], [ 0, 1, 0]]
    (B31, B20, B21, B21) -> [ [ 0, 0,-1], [-1, 0, 0], [ 0, 1, 0]]
    (B31, B21, B20, B20) -> [ [ 1, 0, 0], [ 0, 0, 1], [ 0,-1, 0]]
    (B31, B21, B20, B21) -> [ [ 0, 0,-1], [ 1, 0, 0], [ 0,-1, 0]]
    (B31, B21, B21, B20) -> [ [ 0, 0, 1], [-1, 0, 0], [ 0,-1, 0]]
    (B31, B21, B21, B21) -> [ [-1, 0, 0], [ 0, 0,-1], [ 0,-1, 0]]
    (B32, B20, B20, B20) -> [ [ 0, 1, 0], [ 0, 0, 1], [ 1, 0, 0]]
    (B32, B20, B20, B21) -> [ [ 0, 1, 0], [ 1, 0, 0], [ 0, 0,-1]]
    (B32, B20, B21, B20) -> [ [ 0, 1, 0], [-1, 0, 0], [ 0, 0, 1]]
    (B32, B20, B21, B21) -> [ [ 0, 1, 0], [ 0, 0,-1], [-1, 0, 0]]
    (B32, B21, B20, B20) -> [ [ 0,-1, 0], [ 1, 0, 0], [ 0, 0, 1]]
    (B32, B21, B20, B21) -> [ [ 0,-1, 0], [ 0, 0,-1], [ 1, 0, 0]]
    (B32, B21, B21, B20) -> [ [ 0,-1, 0], [ 0, 0, 1], [-1, 0, 0]]
    (B32, B21, B21, B21) -> [ [ 0,-1, 0], [-1, 0, 0], [ 0, 0,-1]]

-- | Regular matrix multiplication
matMul :: Num n => [[n]] -> [[n]] -> [[n]]
matMul a b = fmap ((`fmap` transpose b) . (sum .) . zipWith (*)) a


-- Props
--
propOriMatrix :: Ori -> Int -> Int -> Expectation
propOriMatrix a i j = oriMatrix a !! i !! j `shouldBe` gtOriMatrix a !! i !! j

propOriMul :: Ori -> Ori -> Int -> Int -> Expectation
propOriMul a b i j = oriMatrix @Int (a `oriMul` b) !! i !! j `shouldBe` oriMatrix a `matMul` oriMatrix b !! i !! j

propOriCS :: Ori -> Int -> Int -> Int -> Expectation
propOriCS a l w d = oriCS a l w d `shouldBe` P3 l' w' d'
  where
    [[l'], [w'], [d']] = oriMatrix a `matMul` [[l],[w],[d]]


-- Test sets

-- | Iterate test function over each Ori value (24)
forAllOris :: Monad m => (Ori -> m b) -> m ()
forAllOris = forM_ allOris

-- | Iterate test function over each Ori pair combination (576)
forAllOriPairs :: Monad m => (Ori -> Ori -> m b) -> m ()
forAllOriPairs f = sequence_ (f <$> allOris <*> allOris)

-- | Iterate test function over each 3x3 matrix coordinate
forAllCoors :: Monad m => (Int -> Int -> m b) -> m ()
forAllCoors f = sequence_ (f <$> [0,1,2] <*> [0,1,2])

-- | Iterate test function over vectors [0..5]^3
forSomeVecs :: Monad m => (Int -> Int -> Int -> m b) -> m ()
forSomeVecs f = sequence_ (f <$> [0..5] <*> [0..5] <*> [0..5])


-- Specs
--
specOriMatrix :: Spec
specOriMatrix = forAllOris $ \a ->
  describe "oriMatrix" $
    forAllCoors $ \i j ->
      describe (" on coordinate " <> show (i,j)) $
        it "converts Ori to matrix" $ propOriMatrix a i j

specOriMul :: Spec
specOriMul = forAllOriPairs $ \a b ->
  describe "oriMul" $
    forAllCoors $ \i j ->
      describe (" on coordinate " <> show (i,j)) $
        it "works the same as matrix multiplication" $ propOriMul a b i j

specOriCS :: Spec
specOriCS = forAllOris $ \a ->
  describe "oriCS" $
    forSomeVecs $ \l w d ->
      describe (" on vector " <> show (l,w,d)) $
        it "is eq to matrix result " $ propOriCS a l w d

spec :: Spec
spec = do
  specOriMatrix
  specOriMul
  specOriCS
