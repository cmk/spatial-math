module Numeric.Spatial.Frame.Ecef  (
    E3
  , e31, e32, e33
  , Ecef(..)
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

-- | A vector in the < https://en.wikipedia.org/wiki/ECEF > frame.
--
-- See also < https://en.wikipedia.org/wiki/World_Geodetic_System >.
--
data Ecef a = Ecef !a !a !a deriving (Eq,Show,Functor)

e31, e32, e33 :: E3 
e31 = F3 I31
e32 = F3 I32
e33 = F3 I33

-- | Vector addition.
--
-- >>> Ecef 1 2 3 <> Ecef 4 5 6
-- Ecef 5 7 9
--
instance (Additive-Semigroup) a => Semigroup (Ecef a) where
  (<>) = mzipWithRep (+)

-- | Matrix addition.
--
instance (Additive-Semigroup) a => Semigroup (Additive (Ecef a)) where
  (<>) = liftR2 $ mzipWithRep (+)

instance (Additive-Monoid) a => Monoid (Ecef a) where
  mempty = pureRep zero

instance (Additive-Monoid) a => Monoid (Additive (Ecef a)) where
  mempty = pure $ pureRep zero

-- | Vector subtraction.
--
-- >>> Ecef 1 2 3 << Ecef 4 5 6
-- Ecef (-3) (-3) (-3)
--
instance (Additive-Group) a => Magma (Ecef a) where
  (<<) = mzipWithRep (-)

-- | Matrix subtraction.
--
-- >>> Nedn (Ecef 1 2 3) (Ecef 4 5 6) (Ecef 7 8 9) << Nedn (Ecef 7 8 9) (Ecef 7 8 9) (Ecef 7 8 9) 
-- Nedn (Ecef (-6) (-6) (-6)) (Ecef (-3) (-3) (-3)) (Ecef 0 0 0)
--
instance (Additive-Group) a => Magma (Additive (Ecef a)) where
  (<<) = liftR2 $ mzipWithRep (-)

instance (Additive-Group) a => Quasigroup (Ecef a)
instance (Additive-Group) a => Quasigroup (Additive (Ecef a))
instance (Additive-Group) a => Loop (Ecef a)
instance (Additive-Group) a => Loop (Additive (Ecef a)) 
instance (Additive-Group) a => Group (Ecef a)
instance (Additive-Group) a => Group (Additive (Ecef a)) 

instance Semiring a => Semimodule a (Ecef a) where
  a *. f = (a *) <$> f
  {-# INLINE (*.) #-}

instance Foldable Ecef where
  foldMap f (Ecef a b c) = f a <> f b <> f c
  {-# INLINE foldMap #-}
  null _ = False
  --length _ = 3

instance Foldable1 Ecef where
  foldMap1 f (Ecef a b c) = f a <> f b <> f c
  {-# INLINE foldMap1 #-}

instance Distributive Ecef where
  distribute f = Ecef (fmap (\(Ecef x _ _) -> x) f) (fmap (\(Ecef _ y _) -> y) f) (fmap (\(Ecef _ _ z) -> z) f)
  {-# INLINE distribute #-}

instance Representable Ecef where
  type Rep Ecef = E3
  tabulate f = Ecef (f e31) (f e32) (f e33)
  {-# InLInE tabulate #-}

  index (Ecef x _ _) (F3 I31) = x
  index (Ecef _ y _) (F3 I32) = y
  index (Ecef _ _ z) (F3 I33) = z
  {-# INLINE index #-}
