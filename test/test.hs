{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

import Data.Semiring
import Data.Functor.Compose

import Numeric.Spatial.Angle
import Numeric.Spatial.Frame
import Numeric.Spatial.Frame.Nedn
import Numeric.Spatial.Frame.Body
import Numeric.Spatial.Frame.Wind
import Numeric.Spatial.Transform
import Numeric.Spatial.Vector

import Numeric.Prelude
import Prelude (IO)
import Control.Category ((>>>),(<<<))

-- Meeting #1

windspeed :: Wind Double
windspeed = Wind 1.0 0.0 0.3

groundspeed :: Nedn Double
groundspeed = Nedn 0.5 0.0 0.0

-- direct matrices
body2nedn1 :: T33 Double 'B 'D
body2nedn1 = tran . Compose $ Nedn (Body 0.0 1.0 0.0) (Body 1.0 0.0 0.0) (Body 0.0 0.0 1.0)

wind2body1 :: T33 Double 'W 'B
wind2body1 = tran . Compose $ Body (Wind 1.0 0.0 0.0) (Wind 0.0 1.0 0.0) (Wind 0.0 0.0 1.0)

wind2nedn1 :: T33 Double 'W 'D 
wind2nedn1 = body2nedn1 >>> wind2body1 

windspeed1 :: Nedn Double
windspeed1 = wind2nedn1 !# windspeed

-- euler angles
eul1, eul2 :: Euler Radian
eul1 = euler (pi/2.0) 0.0 0.0
eul2 = euler (-pi/4.0) 0.0 0.0

body2nedn2 :: T33 Double 'B 'D
body2nedn2 = euler2tran eul1

wind2body2 :: T33 Double 'W 'B
wind2body2 = euler2tran eul2

wind2nedn2 :: T33 Double 'W 'D 
wind2nedn2 = body2nedn2 >>> wind2body2

windspeed2 :: Nedn Double
windspeed2 = wind2nedn2 !# windspeed


-- Meeting #2:

-- 1) What is w' when j is the identity matrix?
-- 2) Do ch 3. ex. 3.4
--
-- 3) For fun, check out this old NASA paper on pitch-roll coupling in VTOL aircraft:
--    https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/20040008096.pdf
--
--    Does j3 resist pitch-roll coupling? Give an example j matrix that would or would not.
--
omega' :: M33 Double -> V3 Double -> V3 Double -> V3 Double
omega' j m w = inv3 j .# (-w `cross` (j .# w) + m)

j1, j2, j3 :: M33 Double
j1 = identity
j2 = m33 1.0 0.0   0.0  0.0 2.0 0.0   0.0  0.0 3.0
j3 = m33 1.0 0.0 (-2.0) 0.0 2.0 0.0 (-2.0) 0.0 3.0

m1, m2 :: V3 Double
m1 = V3 1.0 0.0 5.0
m2 = V3 10.0 0.0 5.0

-- roll, pitch, yaw
w :: V3 Double
w = V3 3.0 0.0 0.0

w' :: V3 Double
w' = omega' j1 m1 w

main :: IO ()
main = return ()
