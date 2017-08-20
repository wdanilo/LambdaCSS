module Control.Monad.Free.List where

import Prologue
import Control.Monad.Trans.Free hiding (wrap)


----------------------
-- === FreeList === --
----------------------

-- === Definition === --

type    MonadFreeList t     = MonadFree (ListCons t)
type    FreeList      t     = FreeListT t Identity
newtype FreeListT     t m a = FreeListT (FreeT (ListCons t) m a)
                              deriving (Applicative, Functor, Monad, MonadFreeList t, MonadTrans)

data ListCons t next = ListCons t next
  deriving (Show, Functor, Foldable, Traversable)

makeLenses          ''FreeListT
deriveBifunctor     ''ListCons
deriveBifoldable    ''ListCons
deriveBitraversable ''ListCons


-- === Utils === --

liftToFreeList :: MonadFreeList t m => t -> m ()
liftToFreeList a = liftF $ ListCons a () ; {-# INLINE liftToFreeList #-}

joinFreeListT :: Monad m => FreeListT t m a -> m (FreeList t a)
joinFreeListT = fmap wrap . joinFreeT . unwrap ; {-# INLINE joinFreeListT #-}

foldrFreeList :: forall t t' a. (t -> t' -> t') -> t' -> FreeList t a -> t'
foldrFreeList f tb lst = foldr f tb (toList lst) ; {-# INLINE foldrFreeList #-}

mapFreeListT :: Monad m => (t -> t') -> FreeListT t m a -> FreeListT t' m a
mapFreeListT f (FreeListT ft) = FreeListT $ go ft where
  go (FreeT ml) = FreeT $ do
    ml <&> \case
      Pure a   -> Pure a
      Free lst -> Free $ bimap f go lst
{-# INLINE mapFreeListT #-}

traverseFreeList :: Applicative f => (t -> f t') -> FreeList t a -> f (FreeList t' a)
traverseFreeList f (FreeListT ft) = fmap FreeListT $ go ft where
  go (FreeT ml) = fmap FreeT $ sequence $ do
    ml <&> \case
      Pure a   -> Pure <$> pure a
      Free lst -> Free <$> bitraverse f go lst
{-# INLINE traverseFreeList #-}


-- === Instances === --

instance PrimMonad m => PrimMonad (FreeListT t m) where
  type PrimState (FreeListT t m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}

type instance Item (FreeList t a) = t
instance t~s => Convertible (FreeList t a) [s] where
  convert = toList . unwrap where
    toList :: Free (ListCons t) a -> [t]
    toList (FreeT (Identity f)) = case f of
      Free (ListCons t f) -> t : toList f
      Pure _              -> mempty
    {-# INLINE toList #-}
  {-# INLINE convert #-}

instance (t~s, Monoid a) => Convertible [s] (FreeList t a) where
  convert = wrap . fromList where
    fromList :: [t] -> Free (ListCons t) a
    fromList = FreeT . Identity . \case
      []     -> Pure mempty
      (t:ts) -> Free . ListCons t $ fromList ts
    {-# INLINE fromList #-}
  {-# INLINE convert #-}

instance Show t => Show (FreeList t a) where show = show . toList ; {-# INLINE show #-}
