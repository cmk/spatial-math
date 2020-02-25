{-# LANGUAGE DataKinds #-}

module Numeric.Spatial.Frame where

import Data.Semimodule.Algebra
import Data.Semimodule.Basis

import Numeric.Prelude
import qualified Control.Monad as M

type T22 a i j = Tran a (F2 j) (F2 i)

type T33 a i j = Tran a (F3 j) (F3 i)

type T44 a i j = Tran a (F4 j) (F4 i)

-- | Data kind distinguishing a physical < https://en.wikipedia.org/wiki/Frame_of_reference >.
--
-- Comments reflect common usage in avionics, but any interpretation may be given.
--
data Frame =
    A
  | B -- ^ Body frame, representing the center of gravity of the body.
  | C
  | D -- ^ < https://en.wikipedia.org/wiki/Local_tangent_plane_coordinates#Local_north,_east,_down_(NED)_coordinates North-East-Down > frame, representing a global origin.
  | E -- ^ < https://en.wikipedia.org/wiki/ECEF Earth-centered, earth-fixed > frame, representing the center of the earth.
  | G
  | H
  | J
  | K
  | L -- ^ Laser frame, representing the physical sensor location.
  | M -- ^ IMU frame, representing the physical sensor location.
  | N 
  | O
  | P
  | Q
  | R -- ^ Rotor frame, representing a rotor hub.
  | S -- ^ Stability frame, representing aircraft heading perturbations wrt wind.
  | T -- ^ Tilt frame for use in differentiating tilt-wing/tilt-rotor frames from cad, rotor, and wind frames
  | U -- ^ < https://en.wikipedia.org/wiki/Local_tangent_plane_coordinates East-North-Up > frame, representing a global origin.
  | W -- ^ Wind frame, representing a particle moving with the local windstream.
  | X
  | Y
  | Z
  deriving Show

-- | A 2-d basis index tagged with a frame of reference.
--
newtype F2 (f :: Frame) = F2 { unF2 :: E2 } deriving (Eq, Show)


-- | A 3-d basis index tagged with a frame of reference.
--
newtype F3 (f :: Frame) = F3 { unF3 :: E3 } deriving (Eq, Show)

instance Semiring r => Algebra r (F3 f) where
  joined = M.join

instance Semiring r => Unital r (F3 f) where
  unital = const

instance Semiring r => Coalgebra r (F3 f) where
  cojoined f = (cojoined $ f . F3) `on` unF3 --(f . _) --coappend (on f unF3) x

instance Semiring r => Counital r (F3 f) where
  counital f = f (F3 E31) + f (F3 E32) + f (F3 E33)

instance Semiring r => Bialgebra r (F3 f)

-- | A 4-d basis index tagged with a frame of reference.
--
newtype F4 (f :: Frame) = F4 { unF4 :: E4 } deriving (Eq, Show)


{-
type A2 = F2 'A
type B2 = F2 'B
type C2 = F2 'C
type D2 = F2 'D
type E2 = F2 'E
type G2 = F2 'G
type H2 = F2 'H
type J2 = F2 'J
type K2 = F2 'K
type L2 = F2 'L
type M2 = F2 'M
type N2 = F2 'N
type O2 = F2 'O
type P2 = F2 'P
type Q2 = F2 'Q
type R2 = F2 'R
type S2 = F2 'S
type T2 = F2 'T
type U2 = F2 'U
type W2 = F2 'W
type X2 = F2 'X
type Y2 = F2 'Y
type Z2 = F2 'Z

type A3 = F3 'A
type B3 = F3 'B
type C3 = F3 'C
type D3 = F3 'D
type E3 = F3 'E
type G3 = F3 'G
type H3 = F3 'H
type J3 = F3 'J
type K3 = F3 'K
type L3 = F3 'L
type M3 = F3 'M
type N3 = F3 'N
type O3 = F3 'O
type P3 = F3 'P
type Q3 = F3 'Q
type R3 = F3 'R
type S3 = F3 'S
type T3 = F3 'T
type U3 = F3 'U
type W3 = F3 'W
type X3 = F3 'X
type Y3 = F3 'Y
type Z3 = F3 'Z

type A4 = F4 'A
type B4 = F4 'B
type C4 = F4 'C
type D4 = F4 'D
type E4 = F4 'E
type G4 = F4 'G
type H4 = F4 'H
type J4 = F4 'J
type K4 = F4 'K
type L4 = F4 'L
type M4 = F4 'M
type N4 = F4 'N
type O4 = F4 'O
type P4 = F4 'P
type Q4 = F4 'Q
type R4 = F4 'R
type S4 = F4 'S
type T4 = F4 'T
type U4 = F4 'U
type W4 = F4 'W
type X4 = F4 'X
type Y4 = F4 'Y
type Z4 = F4 'Z
-}
