module Numeric.Spatial.Frame.Nedn  (
    D3
  , d31, d32, d33
  , Nedn(..)
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

-- | A vector in the < https://en.wikipedia.org/wiki/Local_tangent_plane_coordinates#Local_north,_east,_down_(NED)_coordinates North-East-Down > frame.
--
data Nedn a = Nedn !a !a !a deriving (Eq,Show,Functor)

d31, d32, d33 :: D3 
d31 = F3 I31
d32 = F3 I32
d33 = F3 I33

-- | Vector addition.
--
-- >>> Nedn 1 2 3 <> Nedn 4 5 6
-- Nedn 5 7 9
--
instance (Additive-Semigroup) a => Semigroup (Nedn a) where
  (<>) = mzipWithRep (+)

-- | Matrix addition.
--
instance (Additive-Semigroup) a => Semigroup (Additive (Nedn a)) where
  (<>) = liftR2 $ mzipWithRep (+)

instance (Additive-Monoid) a => Monoid (Nedn a) where
  mempty = pureRep zero

instance (Additive-Monoid) a => Monoid (Additive (Nedn a)) where
  mempty = pure $ pureRep zero

-- | Vector subtraction.
--
-- >>> Nedn 1 2 3 << Nedn 4 5 6
-- Nedn (-3) (-3) (-3)
--
instance (Additive-Group) a => Magma (Nedn a) where
  (<<) = mzipWithRep (-)

-- | Matrix subtraction.
--
-- >>> Cad (Nedn 1 2 3) (Nedn 4 5 6) (Nedn 7 8 9) << Cad (Nedn 7 8 9) (Nedn 7 8 9) (Nedn 7 8 9) 
-- Cad (Nedn (-6) (-6) (-6)) (Nedn (-3) (-3) (-3)) (Nedn 0 0 0)
--
instance (Additive-Group) a => Magma (Additive (Nedn a)) where
  (<<) = liftR2 $ mzipWithRep (-)

instance (Additive-Group) a => Quasigroup (Nedn a)
instance (Additive-Group) a => Quasigroup (Additive (Nedn a))
instance (Additive-Group) a => Loop (Nedn a)
instance (Additive-Group) a => Loop (Additive (Nedn a)) 
instance (Additive-Group) a => Group (Nedn a)
instance (Additive-Group) a => Group (Additive (Nedn a)) 

instance Semiring a => Semimodule a (Nedn a) where
  a *. f = (a *) <$> f
  {-# INLINE (*.) #-}

instance Foldable Nedn where
  foldMap f (Nedn a b c) = f a <> f b <> f c
  {-# INLINE foldMap #-}
  null _ = False
  --length _ = 3

instance Foldable1 Nedn where
  foldMap1 f (Nedn a b c) = f a <> f b <> f c
  {-# INLINE foldMap1 #-}

instance Distributive Nedn where
  distribute f = Nedn (fmap (\(Nedn x _ _) -> x) f) (fmap (\(Nedn _ y _) -> y) f) (fmap (\(Nedn _ _ z) -> z) f)
  {-# INLINE distribute #-}

instance Representable Nedn where
  type Rep Nedn = D3
  tabulate f = Nedn (f d31) (f d32) (f d33)
  {-# InLInE tabulate #-}

  index (Nedn x _ _) (F3 I31) = x
  index (Nedn _ y _) (F3 I32) = y
  index (Nedn _ _ z) (F3 I33) = z
  {-# INLINE index #-}
