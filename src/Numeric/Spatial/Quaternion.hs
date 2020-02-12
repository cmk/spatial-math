module Numeric.Spatial.Quaternion (
    module Numeric.Spatial.Quaternion
  , module Data.Algebra.Quaternion
) where

import Data.Float
import Data.Algebra
import Data.Algebra.Quaternion
import Data.Semiring
import Data.Semifield
import Data.Semimodule
import Numeric.Spatial.Angle
import Numeric.Spatial.Euler
import Numeric.Spatial.Frame
import Numeric.Spatial.Vector
import Numeric.Spatial.Transform
import Numeric.Prelude

withQuat :: Basis (F3 i) f => Basis (F3 j) g => QuatD -> g Double -> f Double
withQuat = app . quat2tran

-- @ 'rotate' = 'app' . 'quat2tran' @
quat2tran :: QuatD -> T33 Double b c
quat2tran q = Tran $ \f -> index3 . vect $ q * Quaternion zero (tabulate3 f) * conj q


-- | Convert a quaternion to its Euler angle representation.
--
-- >>> quat2euler $ quat 1.0 0.0 0.0 0.0
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 0.0}
--
-- >>> quat2euler $ quat irt2 irt2 0 0
-- Euler {eYaw = 0.0, ePitch = -0.0, eRoll = 1.5707963267948966}
--
-- >>> quat2euler $ quat irt2 0.0 0.0 irt2
-- Euler {eYaw = Radian 1.5707963267948966, ePitch = Radian 0.0, eRoll = Radian 0.0}
--
-- >>> quat2euler $ quat irt2 0.0 irt2 0.0 
-- Euler {yaw = 0.0, pitch = 1.5707963057214724, roll = 0.0}
--
quat2euler :: QuatD -> Euler Radian
quat2euler (Quaternion q0 (V3 q1 q2 q3)) = Euler y p r
  where
    r11 = q0 * q0 + q1 * q1 - q2 * q2 - q3 * q3
    r12 = 2.0 * (q1 * q2 + q0 * q3)
    mr13 = -2.0 * (q1 * q3 - q0 * q2)
    r23 = 2.0 * (q2 * q3 + q0 * q1)
    r33 = q0 * q0 - q1 * q1 - q2 * q2 + q3 * q3
    clip = max (-1.0) . min 1.0
    y = rad $ atan2 r12 r11
    p = rad $ asin $ clip mr13 --returns /NaN/ if mr13 is outside [-1, 1]
    r = rad $ atan2 r23 r33

-- | Convert Euler angles to quaternion. The scalar part of the result may be positive or negative.
--
-- >>> euler2quat (Euler 0 0 0)
-- Quaternion 1.0 (V3 0.0 0.0 0.0)
--
-- >>> euler2quat (Euler (pi/2) 0 0)
-- Quaternion 0.7071067811865476 (V3 0.0 0.0 0.7071067811865475)
--
-- >>> euler2quat (Euler 0 (pi/2) 0)
-- Quaternion 0.7071067811865476 (V3 0.0 0.7071067811865475 0.0)
--
-- >>> euler2quat (Euler 0 0 (pi/2))
-- Quaternion 0.7071067811865476 (V3 0.7071067811865475 0.0 0.0)
--
euler2quat :: Euler Radian -> QuatD
euler2quat (Euler yaw pitch roll) = normalize q
  where
    sr2 = sin $ 0.5 * unRad roll
    cr2 = cos $ 0.5 * unRad roll
    sp2 = sin $ 0.5 * unRad pitch
    cp2 = cos $ 0.5 * unRad pitch
    sy2 = sin $ 0.5 * unRad yaw
    cy2 = cos $ 0.5 * unRad yaw
    q0 = cr2*cp2*cy2 + sr2*sp2*sy2
    q1 = sr2*cp2*cy2 - cr2*sp2*sy2
    q2 = cr2*sp2*cy2 + sr2*cp2*sy2
    q3 = cr2*cp2*sy2 - sr2*sp2*cy2

    q = Quaternion q0 (V3 q1 q2 q3)

-- | Obtain a unit length 'Quaternion' representing a rotation of @angle@ radians about @axis@.
--
-- See < https://en.wikipedia.org/wiki/Axis%I2%80%93angle_representation >
--
axisAngle ::  V3 Double -> Radian -> QuatD
axisAngle ax an = normalize $ Quaternion (cos $ unRad an / 2.0) $ sin (unRad an / 2.0) *. ax
{-# INLINE axisAngle #-}

{-
tanhrhs :: QuatD -> QuatD -> QuatD -> QuatD
tanhrhs cai ai d -- = cai * (sin ai / ai) / d
  | d >= -4.2173720203427147e-29 && d < 4.446702369113811e64 = cai / (d # (ai / sin ai))
  | otherwise                                                = cai # (1 N./ ai N./ sin ai) N./ d
-}

-------------------------------------------------------------------------------
-- Some standard unit quaternions
-------------------------------------------------------------------------------

-- | Identity rotation.
--
-- All rotations written according to the < https://en.wikipedia.org/wiki/Right-hand_rule right-hand rule >.
--
q00 :: QuatD
q00 = unit

-- | A \( \pi/2 \) radian rotation about the /y/ axis.
--
-- >>> m33 0 0 1 0 1 0 (-1) 0 0 *. V3 0 1 0 :: V3 Micro
-- V3 0.000000 1.000000 0.000000
--
q01 :: QuatD
q01 = quat irt2 0.0 irt2 0.0

-- | A \( \pi \) radian rotation about the /y/ axis.
--
q02 :: QuatD 
q02 = qj

-- | A \( 3 \pi/2 \) radian rotation about the /y/ axis.
--
q03 :: QuatD 
q03 = quat irt2 0.0 (-irt2) 0.0

-- | A \( \pi/2 \) radian rotation about the /z/ axis.
--
-- >>> rotate q04 $ V3 0 0 1
-- V3 0.000000 0.000000 0.999996
-- >>> rotate q04 $ V3 1 1 0
-- V3 -0.999998 0.999997 0.000000
--
q04 :: QuatD 
q04 = quat irt2 0.0 0.0 irt2

-- | A \( 2 \pi/3 \) radian rotation about the /x-y-z/ axis.
--
-- @ q05 = q01 '*' q04 @
--
-- >>> rotate q05 $ V3 1 1 1
-- V3 1.000000 1.000000 1.000000
-- >>> cosAngle q05 (fmap (irt2*) $ V3 1 (-1) 0)
-- -0.499999999991
-- >>> cosAngle q05 (fmap (irt2*) $ V3 0 1 (-1))
-- -0.499999999991
-- >>> cosAngle q05 (fmap (irt2*irt3*) $ V3 1 (-2) 1)
-- -0.499999999993
--
q05 :: QuatD 
q05 = quat 0.5 0.5 0.5 0.5

-- | A \( \pi \) radian rotation about the /x-y/ axis.
--
-- @ q06 = q02 '*' q04 @
--
-- >>> rotate q06 $ V3 1 1 0
-- V3 0.999997 0.999997 0.000000
-- >>> rotate q06 $ V3 0 0 1
-- V3 0.000000 0.000000 -0.999997
--
q06 :: QuatD 
q06 = quat 0.0 irt2 irt2 0.0

-- | A \( 2 \pi/3 \) radian rotation about the /(-x)-(-y)-z/ axis.
--
-- Equivalent to a \( 3 \pi/2 \) radian rotation about y axis,
-- followed by \( \pi/2 \) radian rotation about z axis:
--
-- @ q07 = q03 '*' q04 @
--
-- >>> rotate q07 $ V3 1 1 (-1)
-- V3 1.000000 1.000000 -1.000000
-- >>> cosAngle q07 ((irt2*irt3*) <$> V3 (-1) 2 1)
-- -0.499999999993
--
q07 :: QuatD
q07 = quat 0.5 (-0.5) (-0.5) 0.5

-- | A \( 3 \pi/2 \) radian rotation about the /z/ axis.
--
-- >>> rotate q08 $ V3 0 0 1
-- V3 0.000000 0.000000 0.999996
-- >>> rotate q08 $ V3 1 1 0
-- V3 0.999997 -0.999997 0.000000
--
q08 :: QuatD
q08 = quat irt2 0.0 0.0 (-irt2)

-- | A \( 2 \pi/3 \) radian rotation about the /(-x)-y-(-z)/ axis.
--
-- @ q09 = q01 '*' q08 @
--
-- >>> rotate q09 $ V3 1 (-1) 1
-- V3 1.000000 -1.000000 1.000000
-- >>> cosAngle q09 ((irt2*irt3*) <$> V3 1 2 1)
-- -0.499999999993
--
q09 :: QuatD
q09 = quat 0.5 (-0.5) 0.5 (-0.5)

-- |
-- @ q10 = q02 '*' q08 @
--
q10 :: QuatD
q10 = quat 0.0 (-irt2) irt2 0.0

-- | A \( 2 \pi/3 \) radian rotation about the /x-(-y)-(-z)/ axis.
--
-- @ q11 = q03 '*' q08 @
--
-- >>> rotate q11 $ V3 (-1) 1 1
-- V3 1.000000 1.000000 -1.000000
-- >>> cosAngle q11 ((irt2*irt3*) <$> V3 1 2 (-1))
-- -0.499999999993
--
q11 :: QuatD
q11 = quat 0.5 0.5 (-0.5) (-0.5)

-- | A \( \pi/2 \) radian rotation about the /x/ axis.
--
-- >>> rotate q12 $ V3 1 0 0
-- V3 0.999996 0.000000 0.000000
-- >>> rotate q12 $ V3 0 1 1
-- V3 0.000000 -0.999998 0.999997
-- 
q12 :: QuatD
q12 = quat irt2 irt2 0.0 0.0

-- |
-- 
-- @ q13 = q01 '*' q12 @
--
q13 :: QuatD
q13 = quat 0.5 0.5 0.5 (-0.5)

-- |
--
-- @ q14 = q02 '*' q12 @
-- 
q14 :: QuatD
q14 = quat 0.0 0.0 irt2 (-irt2)

-- |
--
-- @ q15 = q03 '*' q12 @
--
q15 :: QuatD
q15 = quat 0.5 0.5 (-0.5) 0.5

-- | A \( \pi \) radian rotation about the /x/ axis.
--
-- λ> rotate q16 $ V3 1 0 0
-- V3 1.000000 0.000000 0.000000
-- λ> rotate q16 $ V3 0 1 1
-- V3 0.000000 -1.000000 -1.000000
-- 
q16 :: QuatD
q16 = qi

-- |
--
-- @ q17 = q01 '*' q16 @
--
q17 :: QuatD
q17 = quat 0.0 irt2 0.0 (-irt2)

-- |
--
-- @ q18 = q02 '*' q16 @
--
q18 :: QuatD
q18 = conj qk

-- |
--
-- @ q19 = q03 '*' q16 @
--
q19 :: QuatD
q19 = quat 0.0 irt2 0.0 irt2

-- | A \( 3 \pi/2 \) radian rotation about the /x/ axis.
--
-- >>> rotate q20 $ V3 1 0 0
-- V3 0.999996 0.000000 0.000000
-- >>> rotate q20 $ V3 0 1 1
-- V3 0.000000 0.999997 -0.999997
--
q20 :: QuatD
q20 = quat irt2 (-irt2) 0.0 0.0

-- |
--
-- @ q21 = q01 '*' q20 @
--
q21 :: QuatD
q21 = quat 0.5 (-0.5) 0.5 0.5

-- |
--
-- @ q22 = q02 '*' q20 @
--
q22 :: QuatD
q22 =  quat 0.0 0.0 irt2 irt2

-- |
--
-- @ q23 = q03 '*' q20 @
--
q23 :: QuatD
q23 = quat 0.5 (-0.5) (-0.5) (-0.5)

-------------------------------------------------------------------------------
-- Useful constants for constructing unit quaternions
-------------------------------------------------------------------------------

-- Inverse square root of 2.
-- 0.7071067811865475
irt2 :: Double
irt2 = 1.0 / sqrt 2.0 

-- Inverse square root of 3.
-- 0.5773502691896258
irt3 :: Double
irt3 = 1.0 / sqrt 3.0
