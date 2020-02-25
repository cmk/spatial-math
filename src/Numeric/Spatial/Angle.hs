module Numeric.Spatial.Angle (
    Radian
  , rad
  , unRad
  , Degree
  , deg
  , unDeg
  , Euler(..)
  , euler
  , euler2tran
  , withEuler
) where


import Data.Float
import Data.Semiring

import Numeric.Spatial.Vector
import Numeric.Spatial.Frame
import Numeric.Spatial.Transform
import Numeric.Prelude

-- | An angle expressed in radians.
--
newtype Radian = Rad Double deriving (Eq, Show)

rad :: Double -> Radian
rad x = Rad $ fmod x (pi*2.0)

unRad :: Radian -> Double
unRad (Rad x) = x

-- | An angle expressed in degrees.
--
newtype Degree = Deg Double deriving (Eq, Show)

deg :: Double -> Radian
deg x = Rad $ fmod x 180.0

unDeg :: Degree -> Double
unDeg (Deg x) = x

data Euler a = Euler { eYaw :: a, ePitch :: a, eRoll :: a } deriving (Eq, Functor, Show)

euler :: Double -> Double -> Double -> Euler Radian
euler y p r = rad <$> Euler { eYaw = y, ePitch = p, eRoll = r}

-- | Apply a set of Euler angles.
--
-- >>> eul2 = euler (pi/2.0) 0.0 0.0
-- >>> withEuler eul2 (Nedn 1.0 0.0 0.0) :: Body Double
-- Body (-1.6081226496766366e-16) (-1.0) 0.0
--
withEuler :: Basis (F3 c) f => Basis (F3 b) g => Euler Radian -> g Double -> f Double
withEuler = (!#) . euler2tran

-- | Convert Euler angles to a 3-dimensional DCM.
--
-- >>> eul2 = euler (pi/2.0) 0.0 0.0
-- >>> app . euler2tran $ eul2 (Nedn 1.0 0.0 0.0) :: Body Double
-- Body (-1.6081226496766366e-16) (-1.0) 0.0
--
euler2tran :: Euler Radian -> T33 Double b c 
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

{-
-- Functor and Applicative instance


instance Functor Radian where
    fmap f (Radian x) = Radian (f x)

instance Applicative Radian where
    pure = Radian       
    Radian f <*> r = fmap f r

-- Creating Radian from a value

-- | Convert from radians to degrees.
fromRadian :: (Floating x) => Radian x -> Degree x
fromRadian (Radian x) = Degree (x/pi*180)

-- | Convert from degrees to radians.
fromDegree :: (Floating x) => Degree x -> Radian x
fromDegree (Degree x) = Radian (x/180*pi)

-- | Create a 'Radian' with the given degrees
angleFromDegrees :: (Integral d, Floating r) => d -> Radian r
angleFromDegrees x = Radian $ (realToFrac x) * pi/180


-- Get the value from angle


-- | Get degrees from an angle
angleValueDegrees :: (Floating r, RealFrac r, Integral d) => Radian r -> d
angleValueDegrees (Radian x) = round $ x / pi * 180.0

-- | Get turns from an angle
angleValueTurns :: (Floating r) => Radian r -> r
angleValueTurns (Radian x) = x / (pi*2)

-- Basic functions


-- | Adding two angles
addRadian :: (Floating a) => Radian a -> Radian a -> Radian a
addRadian r1 r2 = (+) <$> r1 <*> r2

-- | Normalize Radian: transforming back to (0-2pi)
normRadian :: (Floating a, Real a) => Radian a -> Radian a
normRadian (Radian r) = Radian $ mod' r (pi*2)

-- | Add two angles and normalize the result
addRadianNorm :: (Floating a, Real a) => Radian a -> Radian a -> Radian a
addRadianNorm a b = normRadian $ addRadian a b

-- | Distance between two angles
distRadian :: (Floating a, Real a) => Radian a -> Radian a -> Radian a
distRadian (Radian r1) (Radian r2) = Radian $ if (a' < b') then a' else b'
    where
        a' = mod' (r1-r2) (pi*2)
        b' = mod' (r2-r1) (pi*2)

-- | Flip angle
flipRadian :: (Floating a) => Radian a -> Radian a
flipRadian = fmap negate

-- | Flip angle and normalize the result
flipRadianNorm :: (Floating a, Real a) => Radian a -> Radian a
flipRadianNorm = normRadian . flipRadian

-- | Add degrees to angle
addRadianDegrees :: (Floating r, Integral d) => Radian r -> d -> Radian r
addRadianDegrees ang deg = addRadian ang $ angleFromDegrees deg

-- | Add radians to angle
addRadianRadian :: (Floating r) => Radian r -> r -> Radian r
addRadianRadian (Radian r1) r2 = Radian $ r1 + r2

-- | Add turns to angle
--addRadianTurns :: (Floating r, Real t) => Radian r -> t -> Radian r
--addRadianTurns ang turn = addRadian ang $ angleFromTurns turn

-- Trigonometric functions


-- | Sine of the angle
sinRadian :: (Floating a) => Radian a -> a
sinRadian = sin . unRadian

-- | Cosine of the angle
cosRadian :: (Floating a) => Radian a -> a
cosRadian = cos . unRadian

-- | Tangent of the angle
tanRadian :: (Floating a) => Radian a -> a
tanRadian = tan . unRadian

-- | Cotangent of the angle
cotRadian :: (Floating a) => Radian a -> a
cotRadian = recip . tan . unRadian


-- Inverse trigonometric functions


-- | Create angle from inverse sine
asinRadian :: (Floating a) => a -> Radian a
asinRadian = Radian . asin

-- | Create angle from inverse cosine
acosRadian :: (Floating a) => a -> Radian a
acosRadian = Radian . acos

-- | Create angle from inverse tangent
atanRadian :: (Floating a) => a -> Radian a
atanRadian = Radian . atan

-- | Create angle from inverse cotangent
acotRadian :: (Floating a) => a -> Radian a
acotRadian x = Radian $ (pi/2) - (atan x)


-- | Create angle from atan2
atan2Radian :: (Floating a, RealFloat a) => a -> a -> Radian a
atan2Radian y x = Radian $ atan2 y x

-}
