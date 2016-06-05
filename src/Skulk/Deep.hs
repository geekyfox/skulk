{-# LANGUAGE CPP #-}

-- | Collection of convenience functions for dealing with nested
-- applicative/monadic/etc  structures.
module Skulk.Deep where

import Control.Applicative((<$>),Applicative,liftA,pure,(<*>))
import Control.Monad(liftM,join)
import Data.Traversable(Traversable, sequenceA)

newtype Deep a b c = Deep { expose :: a (b c) } deriving (Show, Eq)

wrap :: (Monad a) => b c -> Deep a b c
wrap = Deep . return

inject :: (Functor a, Monad b) => a c -> Deep a b c
inject = Deep . fmap return

eject :: (Functor a) => (b c -> c) -> Deep a b c -> a c
eject f = fmap f . expose

instance (Functor a, Functor b) => Functor (Deep a b) where
    f `fmap` (Deep x) = Deep (f <$$> x)

#if __GLASGOW_HASKELL__ < 710
instance (Applicative a, Monad a, Applicative b) => Applicative (Deep a b) where
#else
instance (Monad a, Applicative b) => Applicative (Deep a b) where
#endif
    pure = Deep . pure . pure
    (Deep abf) <*> (Deep abx) = Deep $ do
        bf <- abf
        bx <- abx
        let by = bf <*> bx
        return by

#if __GLASGOW_HASKELL__ < 710
instance (Applicative a, Monad a, Monad b, Traversable b) => Monad (Deep a b) where
#else
instance (Monad a, Monad b, Traversable b) => Monad (Deep a b) where
#endif
    return = Deep . return . return
    fail = Deep . return . fail
    (Deep abx) >>= f = Deep $ do
        bx <- abx
        let baby = expose . f <$> bx
        let abby = sequenceA baby
        let aby = join <$> abby
        aby

-- | Reduces @A (B (A x))@ to @A (B x)@.
reduceABA :: (Applicative a, Monad a, Traversable b) => a (b (a x)) -> a (b x)
reduceABA x = join (sequenceA <$> x)

-- | Reduces @B (A (B x))@ to @A (B x)@.
reduceBAB :: (Applicative a, Traversable b, Monad b) => b (a (b x)) -> a (b x)
reduceBAB x = join <$> sequenceA x

-- | Reduces @A (B (A (B x)))@ to @A (B x)@.
reduceABAB :: (Applicative a, Monad a, Traversable b, Monad b) => a (b (a (b x))) -> a (b x)
reduceABAB x =  join <$> reduceABA x

-- | Reduces @B (A (B (A x)))@ to @A (B x)@.
reduceBABA :: (Applicative a, Monad a, Traversable b, Monad b) => b (a (b (a x))) -> a (b x)
reduceBABA = reduceABA . reduceBAB

-- | \"Deep\" `fmap` for mapping over nested functors.
infixl 4 <$$>
(<$$>) :: (Functor a, Functor b) => (x -> y) -> a (b x) -> a (b y)
f <$$> abx = (\bx -> f <$> bx) <$> abx

-- | Variety of "deep bind" for chaining operations on nested data
-- structures.
infixl 1 >>>=
(>>>=) :: (Applicative a, Monad a, Traversable b, Monad b) => a (b x) -> (x -> a (b y)) -> a (b y)
x >>>= f = reduceABAB (f <$$> x)

-- | Variety of "deep bind" for chaining operations on nested data
-- structures.
infixl 1 >>==
(>>==) :: (Functor a, Functor b, Monad b) => a (b x) -> (x -> b y) -> a (b y)
x >>== f = join <$> (f <$$> x)

-- | Variety of "deep bind" for chaining operations on nested data
-- structures.
infixl 1 >=>=
(>=>=) :: (Applicative a, Monad a, Traversable b) => a (b x) -> (x -> a y) -> a (b y)
x >=>= f = reduceABA (f <$$> x)
