#!/usr/bin/env nix-shell
#! nix-shell -i runghc -p "ghc.withPackages (pkgs: [ pkgs.bytestring pkgs.text pkgs.aeson pkgs.QuickCheck pkgs.containers pkgs.unordered-containers pkgs.generic-random]) " ghcid
#! nix-shell -I nixpkgs=https://github.com/nixos/nixpkgs-channels/archive/nixos-20.03.tar.gz

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Generic.Random
import Test.QuickCheck
import qualified Test.QuickCheck as QC
import Data.Int
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace
import GHC.Generics (Generic)
import Data.List (delete)
import Bank

-- Property of sums
property1 :: Property
property1 =
  property $ \(x :: Word, y :: Word) -> x + y >= x
  --forAll positiveIntegers $ \(x, y) -> x + y >= x
  --where
  --positiveIntegers = do
    --i <- scale (*100000000000000000) arbitrarySizedIntegral
    --j <- scale (*10000) arbitrarySizedIntegral
    --return (i,j)

-- reverse is the inverse of itself
property2 :: Property
property2 =
  property (\(xs :: [Int]) -> reverse (reverse xs) == xs)

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)
  where
  firstHalf = take (n `quot` 2) xs
  secondHalf = drop (n `quot` 2) xs
  n = length xs
  merge xs [] = xs
  merge [] ys = ys
  merge xss@(x:xs) yss@(y:ys)
    | x < y = x:y:merge xs ys
    | otherwise = y:x:merge ys xs

sortProperty :: Property
sortProperty =
  forAll (arbitrary :: Gen [Int])
    (\arr ->
      let
        sorted = mergeSort arr
      in
        fst $ foldl (\(b, prev) this -> (b && prev <= this, this)) (True, minBound) sorted)

main = do
  putStrLn "Hello devsoc!!"
  quickCheck property1
  quickCheck (withMaxSuccess 1000 property2)
  quickCheck transferDoesntChangeTheBalance
  quickCheck depositIncreasesBankBalance
  quickCheck alwaysHaveMoney
  --quickCheck (withMaxSuccess 10000 operationsNotInvolvingCustomerDoNotChangeHisBalance)
  --quickCheck (withMaxSuccess 10000 sortProperty)
