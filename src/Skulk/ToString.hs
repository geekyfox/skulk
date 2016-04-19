{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Collection of functions for dealing with typesafe wrappers around
-- strings (i.e. "@newtype Name = Name String@" etc) and string-like
-- structures alike.
module Skulk.ToString where

import Data.Maybe(fromMaybe,maybe)
import Data.String(IsString,fromString)

-- | Typeclass for a string-like structures that may expose the
-- contained value.
--
-- Typical usage is something along the lines of
--
-- @newtype LastName = LastName String deriving (IsString, ToString)@
class ToString w where
    -- | Exposes string content.
    toString :: w -> String

instance (ToString a, ToString b) => ToString (Either a b) where
    toString = either toString toString

instance ToString String where
    toString = id
    
instance ToString a => ToString (Maybe a) where
    toString = maybe "" toString

-- | Promotes a function.
liftT :: (ToString a, IsString a) => (String -> String) -> a -> a
liftT f = fromString . f . toString

-- | Converts from one string-like structure to another one.
shapeshift :: (ToString a, IsString b) => a -> b
shapeshift = fromString . toString

-- | Converts empty string to `Nothing`.
emptyToNothing :: ToString a => a -> Maybe a
emptyToNothing x | null (toString x) = Nothing | otherwise = Just x

-- | Converts `Nothing` to empty string.
nothingToEmpty :: IsString a => Maybe a -> a
nothingToEmpty = fromMaybe (fromString "")
