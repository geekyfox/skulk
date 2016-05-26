{-# LANGUAGE CPP #-}

module TestOutcome (spec) where

#if __GLASGOW_HASKELL__ < 710
import Data.Traversable(sequenceA)
#endif
import Data.Foldable(toList)

import Test.Hspec
import Test.QuickCheck
import Skulk.Outcome hiding (describe)
import qualified Skulk.Outcome

x, y, z :: Outcome String
x = OK "foobar"
y = Fail "foobar"
z = Skip "foobar"

y', z' :: Outcome Int
y' = Fail "foobar"
z' = Skip "foobar"

impl :: (Show a, Eq a) => (Outcome String -> a) -> a -> a -> a -> Spec
impl f a b c = do
    it "(OK \"foobar\")" $
         f x `shouldBe` a
    it "(Fail \"foobar\")" $
        f y `shouldBe` b
    it "(Skip \"foobar\")" $
        f z `shouldBe` c

toNothing :: Outcome String -> Outcome (Maybe String)
toNothing = fmap (const Nothing)

spec :: Spec
spec = do
    describe "Outcome.Eq" $ do
        it "x == x" $
            x `shouldBe` x
        it "y == y" $
            y `shouldBe` y
        it "z == z" $
            z `shouldBe` z
        it "x /= y" $
            (x == y) `shouldBe` False
        it "x /= z" $
            (x == z) `shouldBe` False
        it "y /= z" $
            (y == z) `shouldBe` False

    describe "Outcome.toEither" $ do
        impl toEither (Right "foobar") (Left "FAIL: foobar") (Left "SKIP: foobar")
    describe "Outcome.show" $ do
        impl show "\"foobar\"" "FAIL: foobar" "SKIP: foobar"
    describe "Outcome.describe id" $ do
        impl (Skulk.Outcome.describe id) "foobar" (show y) (show z)
    describe "Outcome.fmap" $ do
        impl (fmap length) (OK 6) y' z'
    describe "Outcome.sequenceA Just" $ do
        impl (sequenceA . fmap Just) (Just x) (Just y) (Just z)
    describe "Outcome.sequenceA Nothing" $ do
        impl (sequenceA . toNothing) Nothing (Just y) (Just z)
    describe "Outcome.allOK" $ do
        it "[OK, OK]" $ property $ \(a, b) ->
            allOK [OK (a :: Int), OK b] `shouldBe` OK [a, b]
        it "[OK, Skip, OK]" $ property $ \(a, b, c) ->
            allOK [OK (a :: Int), Skip b, OK c] `shouldBe` OK [a, c]
        it "[OK, Fail]" $ property $ \(a, b) ->
            allOK [OK (a :: Int), Fail b] `shouldBe` Fail b
        it "[Fail, OK, Fail]" $ property $ \(a, b, c) ->
            allOK [Fail a, OK (b :: Int), Fail c] `shouldBe` Fail a
    describe "Outcome.fromMaybe" $ do
        it "Nothing" $ property $ \a ->
            fromMaybe a Nothing `shouldBe` (Fail a :: Outcome Int)
        it "Just x" $ property $ \(a, b) ->
            fromMaybe a (Just b) `shouldBe` (OK b :: Outcome Int)
    describe "Outcome.Foldable" $ do
        it "Skip x" $ property $ \a ->
            toList (Skip a) `shouldBe` ([] :: [Int])
        it "Fail x" $ property $ \a ->
            toList (Fail a) `shouldBe` ([] :: [Int])
        it "OK x" $ property $ \a ->
            toList (OK a) `shouldBe` ([a] :: [Int])
    describe "Outcome.fromEither" $ do
        it "Left msg" $ property $ \a ->
            fromEither (Left a :: Either String Int) `shouldBe` (Fail a :: Outcome Int)
        it "Right x" $ property $ \a ->
            fromEither (Right a :: Either String Int) `shouldBe` (OK a :: Outcome Int)

