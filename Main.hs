#!/usr/bin/env nix-shell
#! nix-shell -i runghc -p "ghc.withPackages (pkgs: [ pkgs.bytestring pkgs.text pkgs.aeson pkgs.QuickCheck pkgs.containers pkgs.unordered-containers pkgs.generic-random pkgs.hedgehog]) " ghcid
#! nix-shell -I nixpkgs=https://github.com/nixos/nixpkgs-channels/archive/nixos-20.03.tar.gz

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Word
import Bank
import Debug.Trace

-- Property of sums
propertyReverseIsInverse :: Property
propertyReverseIsInverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.enumBounded :: Gen Int)
    reverse (reverse xs) === xs

propertySumIsLarger :: Property
propertySumIsLarger =
  property $ do
    x <- forAll $ (Gen.enumBounded :: Gen Word32)
    y <- forAll $ Gen.enumBounded
    assert $ x + y >= x

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

mergeSortSorts :: Property
mergeSortSorts =
  property $ do
    xs <- fmap traceShowId $ forAll $ Gen.list (Range.linear 0 1000) (Gen.int $ Range.linear 0 1000 :: Gen Int)
    let
      sorted = mergeSort xs
    assert $ fst $ foldl (\(b, prev) this -> (b && prev <= this, this)) (True, minBound) sorted

mergeSortDoesntRemoveElements :: Property
mergeSortDoesntRemoveElements =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 1 1000) (Gen.int $ Range.linear 0 1000 :: Gen Int)
    x <- forAll $ Gen.element xs
    let
      sorted = mergeSort xs
    assert $ elem x sorted

mergeSortDoesntAddElements :: Property
mergeSortDoesntAddElements =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 1 1000) (Gen.int $ Range.linear 0 1000 :: Gen Int)
    let
      sorted = mergeSort xs
    x <- forAll $ Gen.element sorted
    assert $ elem x xs

main = do
  putStrLn "Hello devsoc!!"
  --check propertyReverseIsInverse
  --check propertySumIsLarger
  --check mergeSortSorts
  --check mergeSortDoesntRemoveElements
  --check mergeSortDoesntAddElements
  --check depositIncreasesBankBalance
  --check transferDoesntChangeTheBalance
  --check transferReducesTheBalance
  --check operationsNotInvolvingCustomerDoNotChangeHisBalance
