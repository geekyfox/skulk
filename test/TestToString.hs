{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestToString (spec) where

import Data.String(IsString)

import Test.Hspec
import Test.QuickCheck
import Skulk.ToString

newtype Dummy = Dummy String deriving (Eq, Show, IsString, ToString)

spec :: Spec
spec = do
    describe "toString" $
        it "Either String String" $ property $ \x -> do
            toString x `shouldBe` (either id id x)
    describe "liftT" $
        it "liftT Dummy" $ property $ \(x, y) -> do
            liftT ((++) x) (Dummy y) `shouldBe` Dummy (x ++ y)
    