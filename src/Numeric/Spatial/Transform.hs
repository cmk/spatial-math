module Numeric.Spatial.Transform (
    tran
  , module Data.Semimodule.Transform
  , module Numeric.Spatial.Transform
) where

import Data.Functor.Rep

import Data.Semimodule.Transform
import Numeric.Spatial.Frame
import Numeric.Spatial.Vector
import Numeric.Spatial.Matrix
import Numeric.Prelude

import Data.Connection

invmap' :: Conn a1 a2 -> Tran a1 b c -> Tran a2 b c
invmap' (Conn f g) = invmap f g
