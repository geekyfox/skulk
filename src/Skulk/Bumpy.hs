-- | Collection of convenience functions for dealing with nested monadic
-- structures.
module Skulk.Bumpy where

import Control.Applicative((<$>),Applicative,liftA,pure,(<*>))
import Control.Monad(liftM,join)

-- | A type class for datatypes that can be \"bumped\" from
-- @Bumpy (Applicative a)@ to @Applicative (Bumpy a)@ .
class Bumpy f where
    bump :: (Applicative m) => f (m a) -> m (f a)

instance Bumpy (Either a) where
    bump = either (pure . Left) (liftA Right)
    
instance Bumpy Maybe where
    bump = maybe (pure Nothing) (liftA Just)

instance Bumpy [] where
    bump = foldr (\x y -> (:) <$> x <*> y) (pure [])

instance Bumpy ((,) a) where
    bump (x,my) = liftA ((,) x) my

-- | Reduces the value of the form @Bumpy (Monad (Bumpy x))@ to the form
-- @Monad (Bumpy x)@
reduceBMB :: (Bumpy b, Monad b, Applicative m) => b (m (b x)) -> m (b x)
reduceBMB = fmap join . bump

-- | Reduces the value of the form @Monad (Bumpy (Monad x))@ to the form
-- @Monad (Bumpy x)@
reduceMBM :: (Bumpy b, Applicative m, Monad m) => m (b (m a)) -> m (b a)
reduceMBM = join . fmap bump

-- | Reduces the value of the form @Monad (Bumpy (Monad (Bumpy x)))@ to
-- the form @Monad (Bumpy x)@
reduceMBMB :: (Bumpy b, Monad b, Applicative m, Monad m) => m (b (m (b a))) -> m (b a)
reduceMBMB =  fmap join . reduceMBM 

-- | Reduces the value of the form @Bumpy (Monad (Bumpy (Monad x)))@ to
-- the form @Monad (Bumpy x)@
reduceBMBM :: (Bumpy b, Monad b, Applicative m, Monad m) => b (m (b (m a))) -> m (b a)
reduceBMBM = reduceMBM . reduceBMB

-- | \"Deep\" `fmap` for mapping over nested functors.
infixl 4 <$$>
(<$$>) :: (Functor m, Functor f) => (a -> b) -> m (f a) -> m (f b)
f <$$> x = fmap (fmap f) x

-- | Variety of "deep bind" for chaining operations on nested data
-- structures.
infixl 1 >>>=
(>>>=) :: (Monad b, Monad m, Bumpy b, Applicative m) => m (b x) -> (x -> m (b y)) -> m (b y)
mbx >>>= f = mbx >>= (reduceBMB . liftM f)

-- | Variety of "deep bind" for chaining operations on nested data
-- structures.
infixl 1 >>==
(>>==) :: (Monad b, Functor m) => m (b x) -> (x -> b y) -> m (b y)
mbx >>== f = fmap (>>= f) mbx

-- | Variety of "deep bind" for chaining operations on nested data
-- structures.
infixl 1 >=>=
(>=>=) :: (Functor b, Bumpy b, Applicative m, Monad m) => m (b x) -> (x -> m y) -> m (b y)
mbx >=>= f = mbx >>= (bump . fmap f)
