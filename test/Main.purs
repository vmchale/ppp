module Test.Main where

import Prelude

import Data.Maybe (Maybe (..))
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert (assert, equal)

import Data.Permutation

main = runTest do
  suite "permutation" do
    test "basics" do
      Just [0, 1, 2, 3] `equal` toArray 4 (identity 4)
    test "pi" do
      Just [0, 3, 2, 1] `equal` toArray 4 (pi 4 1 3)
      Just [2, 1, 0, 3] `equal` toArray 4 (pi 4 0 2)
    test "multiply" do
      Just [2, 3, 0, 1] `equal` (toArray 4 =<< multiply 4 (pi 4 1 3) (pi 4 0 2))
    test "invert" do
      Just [0, 1, 2, 3] `equal` (toArray 4 =<< invert 4 (identity 4))
