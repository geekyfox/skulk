{-# LANGUAGE CPP #-}

module TestDeep (spec) where

import Test.Hspec
import Skulk.Deep

#if __GLASGOW_HASKELL__ < 708
-- Backport from base-4.7.0.0

import Control.Applicative((<$>), pure)
import Data.Foldable(Foldable, foldMap, foldr)
import Data.Traversable(Traversable, traverse)

instance Foldable (Either a) where
    foldr _ z (Left _) = z
    foldr f z (Right y) = f y z

instance Traversable (Either a) where
    traverse _ (Left x) = pure (Left x)
    traverse f (Right y) = Right <$> f y    
#endif

type M a = Maybe a
type E a = Either Int a

validateEM :: E (M String) -> M (E String) -> Spec
validateEM x y = do
    it "reduce BAB to AB" $ do
        reduceBAB (Just x) `shouldBe` x
        reduceBAB (Right y) `shouldBe` y
    it "reduce ABA to AB" $ do
        reduceABA (Just x) `shouldBe` y
        reduceABA (Right y) `shouldBe` x
    it "reduce ABAB to AB" $ do
        reduceABAB (Right $ Just x) `shouldBe` x
        reduceABAB (Just $ Right y) `shouldBe` y
    it "reduce BABA to AB" $ do
        reduceBABA (Right $ Just x) `shouldBe` y
        reduceBABA (Just $ Right y) `shouldBe` x

spec :: Spec
spec = do
    describe "Right.Just vs Just.Right" $ do
        validateEM (Right (Just "foo")) (Just (Right "foo"))
    describe "Left vs Just.Left" $ do
        validateEM (Left 9) (Just (Left 9))
    describe "Right.Nothing vs Nothing" $ do
        validateEM (Right Nothing) Nothing
    describe "[Maybe a] vs Maybe [a]" $ do
        it "reduceBAB [Just [foo]]" $
            reduceBAB [Just ["foo", "bar"], Just ["baz"]] `shouldBe` Just ["foo", "bar", "baz"]
        it "reduceABA [Just [foo]]" $
            reduceABA [Just ["foo", "bar"], Just ["baz"]] `shouldBe` [Just "foo", Just "bar", Just "baz"]
    describe "<$$>" $ do
        it "<$$> over [Maybe String]" $ do
            (++ "bar") <$$> [Just "foo", Nothing] `shouldBe` [Just "foobar", Nothing]
        it "<$$> over [[String]]" $ do
            (++ "x") <$$> [["foo", "bar"]] `shouldBe` [["foox", "barx"]]
    describe ">>>=" $ do
        it ">>>= over [Maybe String]" $ do
            let f x = case x of {
                "foo" -> []
                ; "bar" -> [Nothing]
                ; _ -> [Just x, Just (x ++ "baz")]
                }
            let y = ([Nothing, Just "foo", Just "bar", Just "baz"] >>>= f)
            y `shouldBe` [Nothing, Nothing, Just "baz", Just "bazbaz"]
    describe ">>==" $ do
        it ">>== over [Maybe String]" $ do
            let f x = case x of {
                "foo" -> Nothing
                ; "bar" -> Just "bar"
                ; _ -> Just (x ++ "xxx")
                }
            let y = ([Nothing, Just "foo", Just "bar", Just "baz"] >>== f)
            y `shouldBe` [Nothing, Nothing, Just "bar", Just "bazxxx"]
    describe ">=>=" $ do
        it ">=>= over [Maybe String]" $ do
            let f x = case x of {
                "foo" -> []
                ; "bar" -> ["bar"]
                ; _ -> [x, x ++ "xxx"]
                }
            let y = ([Nothing, Just "foo", Just "bar", Just "baz"] >=>= f)
            y `shouldBe` [Nothing, Just "bar", Just "baz", Just "bazxxx"]
