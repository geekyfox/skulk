module Main (main) where

import Test.Hspec
import qualified TestBumpy
import qualified TestOutcome

main :: IO ()
main = hspec $ do
    describe "Bumpy" TestBumpy.spec
    describe "Outcome" TestOutcome.spec