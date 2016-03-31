-- | Universal result type for calculations that may either: produce
-- a value, signal the failure to obtain value, or signal that value is
-- \"not interesting\".

module Skulk.Outcome where

import Control.Applicative(liftA, Applicative)
import Control.Monad(liftM,ap)

import Skulk.Bumpy

-- | Universal result type for calculations that may either: produce
-- a value, signal the failure to obtain value, or signal that value is
-- \"not interesting\".
--
--   E.g. a text parser distinguishes situations when text file is
--   \"structured enough\" to have a syntax error (that's `Fail`)
--   and when text file is not in a supported format at all
--   (that's `Skip`).
data Outcome a = 
    -- | Result value.
    OK a 
    -- | Failed to obtain value because of particular reason.
  | Fail String
    -- | Depending on context, it's might be \"no action required\" or
    --   \"no action taken\" because of particular reason.
  | Skip String

instance Eq a => Eq (Outcome a) where
    (OK x) == (OK y) = x == y
    (Fail x) == (Fail y) = x == y
    (Skip x) == (Skip y) = x == y
    _ == _ = False
  
instance Functor Outcome where
    fmap f (OK x) = OK (f x)
    fmap _ (Fail msg) = Fail msg
    fmap _ (Skip msg) = Skip msg

instance Applicative Outcome where
    pure = OK
    (OK f) <*> ax = f <$> ax
    (Fail msg) <*> _ = Fail msg
    (Skip msg) <*> _ = Skip msg
  
instance Monad Outcome where
    OK x >>= f = f x
    Fail msg >>= _ = Fail msg
    Skip msg >>= _ = Skip msg
    return = OK
    fail = Fail

instance Bumpy Outcome where
    bump (OK x) = liftA OK x
    bump (Fail msg) = pure $ Fail msg
    bump (Skip msg) = pure $ Skip msg

instance (Show a) => Show (Outcome a) where
    show = describe show

-- | Renders `Outcome` to `String` using provided function to
-- render the `OK`s.
describe :: (a -> String) -> Outcome a -> String
describe f (OK x) = f x
describe _ (Fail msg) = "FAIL: " ++ msg
describe _ (Skip msg) = "SKIP: " ++ msg

-- | Converts `Outcome` into either wrapped value or error message.
toEither :: Outcome a -> Either String a
toEither (OK x) = Right x
toEither (Fail msg) = Left $ "FAIL: " ++ msg
toEither (Skip msg) = Left $ "SKIP: " ++ msg

-- | Collapses a list of `Outcome`s into a single `Outcome`. Result may
-- either be `Fail` (if original list contains any) or list of all
-- `OK`s; all `Skip`s are discarded.
allOK :: [Outcome a] -> Outcome [a]
allOK = impl []
    where impl acc [] = OK $ reverse acc
          impl acc (x:xs) = case x of
              (OK y) -> impl (y:acc) xs
              (Fail msg) -> Fail msg
              (Skip _) -> impl acc xs

-- | Either returns a wrapped value or prints out an error message
--   and terminates the execution with `error`.
exposeOrDie :: Outcome a -> a
exposeOrDie = either error id . toEither
