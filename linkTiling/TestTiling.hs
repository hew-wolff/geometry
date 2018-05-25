{-# LANGUAGE ScopedTypeVariables #-}

module TestTiling where

import Test.HUnit
import qualified Data.Map.Strict as M
import Debug.Trace
import Data.List(sort)
import qualified Data.Set as S

import Small
import Orientation
import Grid
import Tiling as T

main :: IO Counts
main = runTestTT $ TestList [
  let
    options (x0, x1) = case fromEnum x0 of
      0 -> "A"
      1 -> "D"
    ts = ["AD"]
  in (testCase :: LetterCase (Nat2, Nat1)) options ts "pairMatch"
  , let
    options (x0, x1) = case fromEnum x0 of
      0 -> "A"
      1 -> "E"
    ts = []
  in (testCase :: LetterCase (Nat2, Nat1)) options ts "pairNoMatch"
  , let
    options = const allValues
    ts = [
      "AB", "AC", "AD"
      , "EB", "EC", "ED"
      , "BA", "BE"
      , "CA", "CE"
      , "DA", "DE"
      ]
  in (testCase :: LetterCase (Nat1, Nat2)) options ts "pairManyMatches"
  , let
    options (x0, x1) = case (fromEnum x0, fromEnum x1) of
      (0, 0) -> "A"
      (1, 0) -> "B"
      (0, 2) -> "E"
      (1, 2) -> "D"
      _ -> allValues
    ts = [
      "ABEBAD"
      , "ABEBED"
      , "ACEBAD"
      , "ACEBED"
      , "ADEBAD"
      , "ADEBED"
      ]
  in (testCase :: LetterCase (Nat2, Nat3)) options ts "gridManyMatches"
  , let
    options (x0, x1) = case (fromEnum x0, fromEnum x1) of
      (0, 0) -> "A"
      (1, 2) -> "E"
      _ -> allValues
    ts = []
  in (testCase :: LetterCase (Nat2, Nat3)) options ts "gridNoMatches"
  ]

type LetterCase pos = (pos -> [Char]) -> [String] -> String -> Test
testCase :: forall pos0.forall pos1.(
  Enum pos0, Bounded pos0, Ord pos0, Small pos0, Eq pos0, Show pos0
  , Enum pos1, Bounded pos1, Ord pos1, Small pos1, Eq pos1, Show pos1
  ) => LetterCase (pos0, pos1)
testCase options ts label =
  TestCase $ assertEqual label (sort ts) (printedLetterTilings problem) where
    problem = (Problem grid2 options letterFace) :: LetterProblem (pos0, pos1)

type LetterProblem pos = Problem pos (Dir' N2') Char Bool
printedLetterTilings :: (Small pos, Ord pos, Show pos) =>
  LetterProblem pos -> [String]
printedLetterTilings = sort . map T.fromTiling . allTilings where
-- Alternate vowels and consonants, crossword-style.
letterFace tile od = case show od of
  _ : "+" -> vowel
  _  -> not vowel
  where
    vowel = tile `elem` "AEIOU"

instance Small Char where
  allValues = "ABCDE"
