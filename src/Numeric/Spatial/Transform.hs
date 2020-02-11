module Numeric.Spatial.Transform (
    tran
  , module Data.Semimodule.Transform
  , module Numeric.Spatial.Transform
) where

import Data.Functor.Rep
import Data.Semimodule.Basis
import Data.Semimodule.Matrix
import Data.Semimodule.Transform
import Numeric.Spatial.Frame
import Numeric.Prelude

import Data.Connection

invmap' :: Conn a1 a2 -> Tran a1 b c -> Tran a2 b c
invmap' (Conn f g) = invmap f g

index2 :: Basis I2 f => f a -> F2 fr -> a
index2 f = index f . unF2

index2' :: Basis (F2 fr) f => f a -> I2 -> a
index2' f = index f . F2

tabulate2 :: Basis I2 f => (F2 fr -> a) -> f a
tabulate2 f = tabulate $ f . F2

tabulate2' :: Basis (F2 fr) f => (I2 -> a) -> f a
tabulate2' f = tabulate $ f . unF2

dropf2 :: Basis (F2 fr) f => f a -> V2 a
dropf2 = tabulate2 . index

withf2 :: Basis (F2 fr) f => (a -> a -> r) -> f a -> r
withf2 k f = k (index2' f I21) (index2' f I22)

index3 :: Basis I3 f => f a -> F3 fr -> a
index3 f = index f . unF3

index3' :: Basis (F3 fr) f => f a -> I3 -> a
index3' f = index f . F3

tabulate3 :: Basis I3 f => (F3 fr -> a) -> f a
tabulate3 f = tabulate $ f . F3

tabulate3' :: Basis (F3 fr) f => (I3 -> a) -> f a
tabulate3' f = tabulate $ f . unF3

dropf3 :: Basis (F3 fr) f => f a -> V3 a
dropf3 = tabulate3 . index

withf3 :: Basis (F3 fr) f => (a -> a -> a -> r) -> f a -> r
withf3 k f = k (index3' f I31) (index3' f I32) (index3' f I33)

index4 :: Basis I4 f => f a -> F4 fr -> a
index4 f = index f . unF4

index4' :: Basis (F4 fr) f => f a -> I4 -> a
index4' f = index f . F4

tabulate4 :: Basis I4 f => (F4 fr -> a) -> f a
tabulate4 f = tabulate $ f . F4

tabulate4' :: Basis (F4 fr) f => (I4 -> a) -> f a
tabulate4' f = tabulate $ f . unF4

dropf4 :: Basis (F4 fr) f => f a -> V4 a
dropf4 = tabulate4 . index

withf4 :: Basis (F4 fr) f => (a -> a -> a -> a -> r) -> f a -> r
withf4 k f = k (index4' f I41) (index4' f I42) (index4' f I43) (index4' f I44)
