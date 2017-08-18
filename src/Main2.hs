{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Main2 where

import qualified Prelude as P
import Prologue hiding ((>), none, (%=))
-- import Control.Monad.Free
import Control.Monad.Trans.Free hiding (wrap)

instance (PrimMonad m, Functor a) => PrimMonad (FreeT a m) where
  type PrimState (FreeT a m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}


----------------------
-- === FreeList === --
----------------------

-- === Definition === --

type    FreeList  t     = FreeListT t Identity
newtype FreeListT t m a = FreeListT (FreeT (ListCons t) m a) deriving (Applicative, Functor, Monad, MonadFree (ListCons t), MonadTrans)

data ListCons a next = ListCons a next
  deriving (Show, Functor, Foldable, Traversable)

makeLenses ''FreeListT


-- === Utils === --

liftToFreeList :: MonadFree (ListCons a) m => a -> m ()
liftToFreeList a = liftF $ ListCons a ()


-- === Instances === --

instance PrimMonad m => PrimMonad (FreeListT t m) where
  type PrimState (FreeListT t m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}

type instance Item (FreeList t a) = t
instance t~s => Convertible (FreeList t a) [s] where
  convert = toList . unwrap where
    toList :: Free (ListCons t) a -> [t]
    toList (FreeT (Identity f)) = case f of
      Pure _ -> mempty
      Free (ListCons t f) -> t : toList f

instance Show t => Show (FreeList t a) where show = show . toList




tst :: (PrimMonadIO m, MonadFree (ListCons Int) m) => m ()
tst = do
  liftToFreeList 1
  print "hello"
  liftToFreeList 1
  liftToFreeList 1

joinFreeListT :: (Monad m) => FreeListT t m a -> m (FreeList t a)
joinFreeListT = fmap wrap . joinFreeT . unwrap

tst2 :: PrimMonadIO m => m (FreeList Int ())
tst2 = joinFreeListT tst

test = do
  a <- tst2
  print a
  print "hello!"
