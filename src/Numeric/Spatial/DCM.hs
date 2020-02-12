module Numeric.Spatial.DCM where

import Data.Float.Unit
import Data.Semimodule
import Numeric.Spatial.Vector
import Numeric.Spatial.Frame
--import Numeric.Spatial.Quaternion
import Numeric.Prelude

-- | A 2-dimensional < https://en.wikipedia.org/wiki/Euclidean_vector#Conversion_between_multiple_Cartesian_bases DCM transform >.
--
-- See also https://en.wikipedia.org/wiki/Rotation_formalisms_in_three_dimensions .
--
type DCM2 f g = forall i j . Basis (F2 i) f => Basis (F2 j) g => f (g Biunit)

-- | A 3-dimensional DCM
type DCM3 f g = forall i j . Basis (F3 i) f => Basis (F3 j) g => f (g Biunit)

-- | a 4-dimensional DCM
type DCM4 f g = forall i j . Basis (F4 i) f => Basis (F4 j) g => f (g Biunit)


-- | A 3-dimensional DCM
--
-- /Caution/ does not check for orthogonality.
--
dcm :: forall f g. V3 (f Double) -> V3 (g Double) -> DCM3 f g
dcm (V3 f1 f2 f3) (V3 g1 g2 g3) = mat
  where inn x y = biunit' $ dropF3 @_ @f x .*. dropF3 @_ @g y 
        [a,b,c,d,e,f,g,h,i] = liftA2 inn [f1, f2, f3] [g1, g2, g3]
        mat = fillF3 (fillF3 a b c) (fillF3 d e f) (fillF3 g h i)

-- | convert a DCM to a quaternion
--
-- >>> quatOfDcm $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
-- Quaternion 1.0 (V3 0.0 0.0 0.0)
--
-- >>> quatOfDcm $ V3 (V3 0 1 0) (V3 (-1) 0 0) (V3 0 0 1)
-- Quaternion 0.7071067811865476 (V3 0.0 0.0 0.7071067811865475)
--
-- >>> let s = sqrt(2)/2 in quatOfDcm $ V3 (V3 s s 0) (V3 (-s) s 0) (V3 0 0 1)
-- Quaternion 0.9238795325112867 (V3 0.0 0.0 0.3826834323650898)
{-
quatOfDcm :: DCM3 -> QuatD
quatOfDcm = 
  (V3
    (V3 r11 r12 r13)
    (V3 r21 r22 r23)
    (V3 r31 r32 r33)
  )
  | r11 + r22 + r33 > 0 =
      let sqtrp1 = sqrt (r11 + r22 + r33 + 1)
          q0 = 0.5*sqtrp1
          qx = (r23 - r32)/(2.0*sqtrp1)
          qy = (r31 - r13)/(2.0*sqtrp1)
          qz = (r12 - r21)/(2.0*sqtrp1)
      in Quaternion q0 (V3 qx qy qz)
  | (r22 > r11) && (r22 > r33) =
      let -- max value at r22
          sqdip1' = sqrt (r22 - r11 - r33 + 1)

          qy = 0.5*sqdip1'

          sqdip1
            | sqdip1' == 0 = 0
            | otherwise = 0.5/sqdip1'

          q0 = (r31 - r13)*sqdip1
          qx = (r12 + r21)*sqdip1
          qz = (r23 + r32)*sqdip1

      in Quaternion q0 (V3 qx qy qz)
  | r33 > r11 =
      let -- max value at r33
          sqdip1' = sqrt (r33 - r11 - r22 + 1)

          qz = 0.5*sqdip1'

          sqdip1
            | sqdip1' == 0 = 0
            | otherwise = 0.5/sqdip1'

          q0 = (r12 - r21)*sqdip1
          qx = (r31 + r13)*sqdip1
          qy = (r23 + r32)*sqdip1

      in Quaternion q0 (V3 qx qy qz)
  | otherwise =
      let -- max value at r11
          sqdip1' = sqrt (r11 - r22 - r33 + 1)

          qx = 0.5*sqdip1'

          sqdip1
            | sqdip1' == 0 = 0
            | otherwise = 0.5/sqdip1'

          q0 = (r23 - r32)*sqdip1
          qy = (r12 + r21)*sqdip1
          qz = (r31 + r13)*sqdip1

      in Quaternion q0 (V3 qx qy qz)
-}
