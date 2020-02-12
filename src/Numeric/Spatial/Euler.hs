module Numeric.Spatial.Euler where

import Data.Float
import Data.Semiring
import Numeric.Spatial.Angle
import Numeric.Spatial.Vector
import Numeric.Spatial.Frame
import Numeric.Spatial.Matrix
import Numeric.Spatial.Transform
import Numeric.Prelude

data Euler a = Euler { eYaw :: a, ePitch :: a, eRoll :: a } deriving (Eq, Functor, Show)

euler :: Double -> Double -> Double -> Euler Radian
euler y p r = rad <$> Euler { eYaw = y, ePitch = p, eRoll = r}

-- | Apply a set of Euler angles.
--
-- >>> eul2 = euler (pi/2.0) 0.0 0.0
-- >>> withEuler eul2 (Nedn 1.0 0.0 0.0) :: Body Double
-- Body (-1.6081226496766366e-16) (-1.0) 0.0
--
withEuler :: Basis (F3 i) f => Basis (F3 j) g => Euler Radian -> g Double -> f Double
withEuler = app . euler2tran

-- | Convert Euler angles to a 3-dimensional DCM.
--
euler2tran :: Euler Radian -> T33 Double i j
euler2tran x = Tran $ \f -> index3 $ euler2matrix x .# tabulate3 f

-- | Convert Euler angles to a 3x3 matrix.
--
-- >>> euler2matrix $ euler 0.0 0.0 0.0
-- V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)
-- >>> euler2matrix $ euler (pi/2.0) 0.0 0.0
-- V3 (V3 (-1.6081226496766366e-16) 1.0 0.0) (V3 (-1.0) (-1.6081226496766366e-16) 0.0) (V3 0.0 0.0 1.0)
-- >>> euler2matrix $ euler (pi/4.0) 0.0 0.0
-- V3 (V3 0.7071067811865475 0.7071067811865476 0.0) (V3 (-0.7071067811865476) 0.7071067811865475 0.0) (V3 0.0 0.0 1.0)
--
euler2matrix :: Euler Radian -> M33 Double
euler2matrix x = mat where
    cPs = cos . unRad $ eYaw x
    sPs = sin . unRad $ eYaw x
    cTh = cos . unRad $ ePitch x
    sTh = sin . unRad $ ePitch x
    cPh = cos . unRad $ eRoll x
    sPh = sin . unRad $ eRoll x
    mat = m33 (cTh * cPs) (cTh * sPs) (- sTh)
              (cPs * sTh * sPh - cPh * sPs) (cPh * cPs + sTh * sPh * sPs) (cTh * sPh)
              (cPh * cPs * sTh + sPh * sPs) (- cPs * sPh + cPh * sTh * sPs) (cTh * cPh)
