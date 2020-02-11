module Numeric.Spatial.DCM where

import Data.Float.Unit
import Data.Semimodule.Basis
import Numeric.Spatial.Frame

-- | A 2-dimensional < https://en.wikipedia.org/wiki/Euclidean_vector#Conversion_between_multiple_Cartesian_bases DCM transform >.
--
-- See also https://en.wikipedia.org/wiki/Rotation_formalisms_in_three_dimensions .
--
type DCM2 i j = forall f g. Basis (F2 i) f => Basis (F2 j) g => f (g Biunit)

-- | A 3-dimensional DCM
type DCM3 i j = forall f g. Basis (F3 i) f => Basis (F3 j) g => f (g Biunit)

-- | a 4-dimensional DCM
type DCM4 i j = forall f g. Basis (F2 i) f => Basis (F2 j) g => f (g Biunit)
