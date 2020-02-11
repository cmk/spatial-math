module Numeric.Spatial.Angle where

import Numeric.Prelude

-- | An angle expressed in radians.
--
newtype Radian = Radian Double deriving (Eq, Show)

unRadian :: Radian -> Double
unRadian (Radian x) = x

rad :: Double -> Radian
rad x = Radian $ fmod x (pi*2.0)

-- | An angle expressed in degrees.
--
newtype Degree x = Degree x deriving (Eq, Show)



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
