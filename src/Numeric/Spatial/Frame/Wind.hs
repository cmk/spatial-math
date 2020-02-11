module Numeric.Spatial.Frame.Wind (
    W3
  , w31, w32, w33
  , Wind(..)
) where

import Data.Distributive
import Data.Foldable
import Data.Functor.Rep
import Data.Semigroup.Foldable as Foldable1
import Data.Semimodule
import Data.Semimodule.Basis
import Data.Semiring
import Numeric.Prelude
import Numeric.Spatial.Frame

-- | A vector in the wind frame.
--
data Wind a = Wind !a !a !a deriving (Eq,Show,Functor)

w31, w32, w33 :: W3 
w31 = F3 I31
w32 = F3 I32
w33 = F3 I33

-- | Vector addition.
--
-- >>> Wind 1 2 3 <> Wind 4 5 6
-- Wind 5 7 9
--
instance (Additive-Semigroup) a => Semigroup (Wind a) where
  (<>) = mzipWithRep (+)

-- | Matrix addition.
--
instance (Additive-Semigroup) a => Semigroup (Additive (Wind a)) where
  (<>) = liftR2 $ mzipWithRep (+)

instance (Additive-Monoid) a => Monoid (Wind a) where
  mempty = pureRep zero

instance (Additive-Monoid) a => Monoid (Additive (Wind a)) where
  mempty = pure $ pureRep zero

-- | Vector subtraction.
--
-- >>> Wind 1 2 3 << Wind 4 5 6
-- Wind (-3) (-3) (-3)
--
instance (Additive-Group) a => Magma (Wind a) where
  (<<) = mzipWithRep (-)

-- | Matrix subtraction.
--
-- >>> Body (Wind 1 2 3) (Wind 4 5 6) (Wind 7 8 9) << Body (Wind 7 8 9) (Wind 7 8 9) (Wind 7 8 9) 
-- Body (Wind (-6) (-6) (-6)) (Wind (-3) (-3) (-3)) (Wind 0 0 0)
--
instance (Additive-Group) a => Magma (Additive (Wind a)) where
  (<<) = liftR2 $ mzipWithRep (-)

instance (Additive-Group) a => Quasigroup (Wind a)
instance (Additive-Group) a => Quasigroup (Additive (Wind a))
instance (Additive-Group) a => Loop (Wind a)
instance (Additive-Group) a => Loop (Additive (Wind a)) 
instance (Additive-Group) a => Group (Wind a)
instance (Additive-Group) a => Group (Additive (Wind a)) 

instance Semiring a => Semimodule a (Wind a) where
  a *. f = (a *) <$> f
  {-# INLINE (*.) #-}

instance Foldable Wind where
  foldMap f (Wind a b c) = f a <> f b <> f c
  {-# INLINE foldMap #-}
  null _ = False
  --length _ = 3

instance Foldable1 Wind where
  foldMap1 f (Wind a b c) = f a <> f b <> f c
  {-# INLINE foldMap1 #-}

instance Distributive Wind where
  distribute f = Wind (fmap (\(Wind x _ _) -> x) f) (fmap (\(Wind _ y _) -> y) f) (fmap (\(Wind _ _ z) -> z) f)
  {-# INLINE distribute #-}

instance Representable Wind where
  type Rep Wind = W3
  tabulate f = Wind (f w31) (f w32) (f w33)
  {-# INLINE tabulate #-}

  index (Wind x _ _) (F3 I31) = x
  index (Wind _ y _) (F3 I32) = y
  index (Wind _ _ z) (F3 I33) = z
  {-# INLINE index #-}
