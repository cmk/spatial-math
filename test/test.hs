{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

import Data.Float
import Data.Semimodule.Basis
import Data.Semimodule.Matrix
import Data.Semiring

import Numeric.Spatial.Angle
import Numeric.Spatial.Euler
import Numeric.Spatial.Frame
import Numeric.Spatial.Frame.Nedn
import Numeric.Spatial.Frame.Body
import Numeric.Spatial.Frame.Wind
import Numeric.Spatial.Quaternion (q08, q12, quat2tran)

import Numeric.Spatial.Transform
import Numeric.Prelude
import Prelude (IO)
import Data.Connection
import Data.Float.Unit
import Control.Category ((>>>),(<<<))

import qualified Control.Category as C

windspeed :: Wind Double
windspeed = Wind 1.0 0.0 0.3

groundspeed :: Nedn Double
groundspeed = Nedn 0.5 0.0 0.0

-- direct matrices
body2nedn1 :: T33 Double 'B 'D
body2nedn1 = tran $ Nedn (Body 0.0 1.0 0.0) (Body 1.0 0.0 0.0) (Body 0.0 0.0 1.0)

wind2body1 :: T33 Double 'W 'B
wind2body1 = tran $ Body (Wind 1.0 0.0 0.0) (Wind 0.0 1.0 0.0) (Wind 0.0 0.0 1.0)

wind2nedn1 :: T33 Double 'W 'D 
wind2nedn1 = body2nedn1 >>> wind2body1 

windspeed1 :: Nedn Double
windspeed1 = app wind2nedn1 windspeed

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
windspeed2 = app wind2nedn2 windspeed

-- quaternions
body2nedn3 :: T33 Double 'B 'D
body2nedn3 = quat2tran q08

wind2body3 :: T33 Double 'W 'B
wind2body3 = quat2tran q12

wind2nedn3 :: T33 Double 'W 'D 
wind2nedn3 = body2nedn3 >>> wind2body3

windspeed3 :: Nedn Double
windspeed3 = app wind2nedn3 windspeed

main :: IO ()
main = return ()
