module TestOutcome (spec) where
    
import Test.Hspec
import Skulk.Bumpy
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
    describe "Outcome.bump Just" $ do
        impl (bump . fmap Just) (Just x) (Just y) (Just z)
    describe "Outcome.bump Nothing" $ do
        impl (bump . toNothing) Nothing (Just y) (Just z)

        