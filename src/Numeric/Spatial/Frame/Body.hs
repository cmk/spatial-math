module Numeric.Spatial.Frame.Body (
    B3
  , b31, b32, b33
  , Body(..)
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

-- | A vector in the body frame.
--
data Body a = Body !a !a !a deriving (Eq,Show,Functor)

b31, b32, b33 :: B3 
b31 = F3 I31
b32 = F3 I32
b33 = F3 I33

-- | Vector addition.
--
-- >>> Body 1 2 3 <> Body 4 5 6
-- Body 5 7 9
--
instance (Additive-Semigroup) a => Semigroup (Body a) where
  (<>) = mzipWithRep (+)

-- | Matrix addition.
--
instance (Additive-Semigroup) a => Semigroup (Additive (Body a)) where
  (<>) = liftR2 $ mzipWithRep (+)

instance (Additive-Monoid) a => Monoid (Body a) where
  mempty = pureRep zero

instance (Additive-Monoid) a => Monoid (Additive (Body a)) where
  mempty = pure $ pureRep zero

-- | Vector subtraction.
--
-- >>> Body 1 2 3 << Body 4 5 6
-- Body (-3) (-3) (-3)
--
instance (Additive-Group) a => Magma (Body a) where
  (<<) = mzipWithRep (-)

-- | Matrix subtraction.
--
-- >>> Nedn (Body 1 2 3) (Body 4 5 6) (Body 7 8 9) << Nedn (Body 7 8 9) (Body 7 8 9) (Body 7 8 9) 
-- Nedn (Body (-6) (-6) (-6)) (Body (-3) (-3) (-3)) (Body 0 0 0)
--
instance (Additive-Group) a => Magma (Additive (Body a)) where
  (<<) = liftR2 $ mzipWithRep (-)

instance (Additive-Group) a => Quasigroup (Body a)
instance (Additive-Group) a => Quasigroup (Additive (Body a))
instance (Additive-Group) a => Loop (Body a)
instance (Additive-Group) a => Loop (Additive (Body a)) 
instance (Additive-Group) a => Group (Body a)
instance (Additive-Group) a => Group (Additive (Body a)) 

instance Semiring a => Semimodule a (Body a) where
  a *. f = (a *) <$> f
  {-# INLINE (*.) #-}

instance Foldable Body where
  foldMap f (Body a b c) = f a <> f b <> f c
  {-# INLINE foldMap #-}
  null _ = False
  --length _ = 3

instance Foldable1 Body where
  foldMap1 f (Body a b c) = f a <> f b <> f c
  {-# INLINE foldMap1 #-}

instance Distributive Body where
  distribute f = Body (fmap (\(Body x _ _) -> x) f) (fmap (\(Body _ y _) -> y) f) (fmap (\(Body _ _ z) -> z) f)
  {-# INLINE distribute #-}

instance Representable Body where
  type Rep Body = B3
  tabulate f = Body (f b31) (f b32) (f b33)
  {-# INLINE tabulate #-}

  index (Body x _ _) (F3 I31) = x
  index (Body _ y _) (F3 I32) = y
  index (Body _ _ z) (F3 I33) = z
  {-# INLINE index #-}
