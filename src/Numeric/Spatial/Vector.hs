module Numeric.Spatial.Vector (
    module Data.Semimodule.Free
  , module Numeric.Spatial.Vector
) where

import Data.Functor.Rep
import Data.Semimodule
import Data.Semimodule.Free
import Data.Semimodule.Basis
import Numeric.Spatial.Frame
import Numeric.Prelude


fillF3 :: Basis (F3 fr) f => a -> a -> a -> f a
fillF3 x y z = tabulate3' $ fillE3 x y z

index2 :: Basis E2 f => f a -> F2 fr -> a
index2 f = index f . unF2

index2' :: Basis (F2 fr) f => f a -> E2 -> a
index2' f = index f . F2

tabulate2 :: Basis E2 f => (F2 fr -> a) -> f a
tabulate2 f = tabulate $ f . F2

tabulate2' :: Basis (F2 fr) f => (E2 -> a) -> f a
tabulate2' f = tabulate $ f . unF2

dropF2 :: Basis (F2 fr) f => f a -> V2 a
dropF2 = tabulate2 . index

withF2 :: Basis (F2 fr) f => (a -> a -> r) -> f a -> r
withF2 k f = k (index2' f E21) (index2' f E22)

index3 :: Basis E3 f => f a -> F3 fr -> a
index3 f = index f . unF3

index3' :: Basis (F3 fr) f => f a -> E3 -> a
index3' f = index f . F3

tabulate3 :: Basis E3 f => (F3 fr -> a) -> f a
tabulate3 f = tabulate $ f . F3

tabulate3' :: Basis (F3 fr) f => (E3 -> a) -> f a
tabulate3' f = tabulate $ f . unF3

dropF3 :: Basis (F3 fr) f => f a -> V3 a
dropF3 = tabulate3 . index

withF3 :: Basis (F3 fr) f => (a -> a -> a -> r) -> f a -> r
withF3 k f = k (index3' f E31) (index3' f E32) (index3' f E33)

index4 :: Basis E4 f => f a -> F4 fr -> a
index4 f = index f . unF4

index4' :: Basis (F4 fr) f => f a -> E4 -> a
index4' f = index f . F4

tabulate4 :: Basis E4 f => (F4 fr -> a) -> f a
tabulate4 f = tabulate $ f . F4

tabulate4' :: Basis (F4 fr) f => (E4 -> a) -> f a
tabulate4' f = tabulate $ f . unF4

dropF4 :: Basis (F4 fr) f => f a -> V4 a
dropF4 = tabulate4 . index

withF4 :: Basis (F4 fr) f => (a -> a -> a -> a -> r) -> f a -> r
withF4 k f = k (index4' f E41) (index4' f E42) (index4' f E43) (index4' f E44)
