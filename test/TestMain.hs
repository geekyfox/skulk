module Main (main) where

import Test.Hspec
import qualified TestDeep
import qualified TestOutcome
import qualified TestToString

main :: IO ()
main = hspec $ do
    describe "Deep" TestDeep.spec
    describe "Outcome" TestOutcome.spec
    describe "ToString" TestToString.spec