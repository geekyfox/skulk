module Main (main) where

import Test.Hspec
import qualified TestDeep
import qualified TestOutcome

main :: IO ()
main = hspec $ do
    describe "Deep" TestDeep.spec
    describe "Outcome" TestOutcome.spec