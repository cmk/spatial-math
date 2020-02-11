module Numeric.Spatial.Frame.Enup  (
    U3
  , u31, u32, u33
  , Enup(..)
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

-- | A vector in the < https://en.wikipedia.org/wiki/Local_tangent_plane_coordinates North-East-Down > frame.
--
data Enup a = Enup !a !a !a deriving (Eq,Show,Functor)

u31, u32, u33 :: U3 
u31 = F3 I31
u32 = F3 I32
u33 = F3 I33

-- | Vector addition.
--
-- >>> Enup 1 2 3 <> Enup 4 5 6
-- Enup 5 7 9
--
instance (Additive-Semigroup) a => Semigroup (Enup a) where
  (<>) = mzipWithRep (+)

-- | Matrix addition.
--
instance (Additive-Semigroup) a => Semigroup (Additive (Enup a)) where
  (<>) = liftR2 $ mzipWithRep (+)

instance (Additive-Monoid) a => Monoid (Enup a) where
  mempty = pureRep zero

instance (Additive-Monoid) a => Monoid (Additive (Enup a)) where
  mempty = pure $ pureRep zero

-- | Vector subtraction.
--
-- >>> Enup 1 2 3 << Enup 4 5 6
-- Enup (-3) (-3) (-3)
--
instance (Additive-Group) a => Magma (Enup a) where
  (<<) = mzipWithRep (-)

-- | Matrix subtraction.
--
-- >>> Nedn (Enup 1 2 3) (Enup 4 5 6) (Enup 7 8 9) << Nedn (Enup 7 8 9) (Enup 7 8 9) (Enup 7 8 9) 
-- Nedn (Enup (-6) (-6) (-6)) (Enup (-3) (-3) (-3)) (Enup 0 0 0)
--
instance (Additive-Group) a => Magma (Additive (Enup a)) where
  (<<) = liftR2 $ mzipWithRep (-)

instance (Additive-Group) a => Quasigroup (Enup a)
instance (Additive-Group) a => Quasigroup (Additive (Enup a))
instance (Additive-Group) a => Loop (Enup a)
instance (Additive-Group) a => Loop (Additive (Enup a)) 
instance (Additive-Group) a => Group (Enup a)
instance (Additive-Group) a => Group (Additive (Enup a)) 

instance Semiring a => Semimodule a (Enup a) where
  a *. f = (a *) <$> f
  {-# INLINE (*.) #-}

instance Foldable Enup where
  foldMap f (Enup a b c) = f a <> f b <> f c
  {-# INLINE foldMap #-}
  null _ = False
  --length _ = 3

instance Foldable1 Enup where
  foldMap1 f (Enup a b c) = f a <> f b <> f c
  {-# INLINE foldMap1 #-}

instance Distributive Enup where
  distribute f = Enup (fmap (\(Enup x _ _) -> x) f) (fmap (\(Enup _ y _) -> y) f) (fmap (\(Enup _ _ z) -> z) f)
  {-# INLINE distribute #-}

instance Representable Enup where
  type Rep Enup = U3
  tabulate f = Enup (f u31) (f u32) (f u33)
  {-# InLInE tabulate #-}

  index (Enup x _ _) (F3 I31) = x
  index (Enup _ y _) (F3 I32) = y
  index (Enup _ _ z) (F3 I33) = z
  {-# INLINE index #-}
