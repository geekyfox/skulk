module TestBumpy (spec) where

import Test.Hspec
import Skulk.Bumpy

type M a = Maybe a
type E a = Either Int a
type T a = (String, a)

validateEM :: E (M String) -> M (E String) -> Spec
validateEM x y = do
    it "bump first is second" $
        bump x `shouldBe` y
    it "bump second is first" $
        bump y `shouldBe` x
    it "reduce BMB to MB" $ do
        reduceBMB (Just x) `shouldBe` x
        reduceBMB (Right y) `shouldBe` y
    it "reduce MBM to MB" $ do
        reduceMBM (Just x) `shouldBe` y
        reduceMBM (Right y) `shouldBe` x
    it "reduce MBMB to MB" $ do
        reduceMBMB (Right $ Just x) `shouldBe` x
        reduceMBMB (Just $ Right y) `shouldBe` y
    it "reduce BMBM to MB" $ do
        reduceBMBM (Right $ Just x) `shouldBe` y
        reduceBMBM (Just $ Right y) `shouldBe` x

validateTM :: T (M Int) -> M (T Int) -> Spec
validateTM x y = do
    it "bump first is second" $
        bump x `shouldBe` y

spec :: Spec
spec = do
    describe "Right.Just vs Just.Right" $ do
        validateEM (Right (Just "foo")) (Just (Right "foo"))
    describe "Left vs Just.Left" $ do
        validateEM (Left 9) (Just (Left 9))
    describe "Right.Nothing vs Nothing" $ do
        validateEM (Right Nothing) Nothing
    describe "(, Just) vs Just (,)" $ do
        validateTM ("foo", Just 7) (Just ("foo", 7))
    describe "(, Nothing) vs Nothing" $ do
        validateTM ("foo", Nothing) Nothing
    describe "[[a]]" $ do
        it "bump [[a]]" $ do
            bump [["foo", "bar"], ["baz"]] `shouldBe` [["foo", "baz"], ["bar", "baz"]]
            bump [["foo", "bar"], []] `shouldBe` []
    describe "[Maybe a] vs Maybe [a]" $ do
        it "bump [Just]" $ do
            bump [Just "foo"] `shouldBe` Just ["foo"]
        it "bump [Just, Just]" $ do
            bump [Just "foo", Just "bar"] `shouldBe` Just ["foo", "bar"]
        it "bump [Just, Nothing]" $
            bump [Just "foo", Nothing] `shouldBe` Nothing
        it "bump []" $
            bump ([] :: [Maybe String]) `shouldBe` Just []
        it "reduceBMB [Just [foo]]" $
            reduceBMB [Just ["foo", "bar"], Just ["baz"]] `shouldBe` Just ["foo", "bar", "baz"]
        it "reduceMBM [Just [foo]]" $
            reduceMBM [Just ["foo", "bar"], Just ["baz"]] `shouldBe` [Just "foo", Just "bar", Just "baz"]
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
