{- |
A numeric types to manage engineering units.

There are three distinct types: Unit, Value, and Quantity.

    * A `Unit` describes a combination of SI units and a conversion factor.
    * A `Value` is a numeric value associated with a`Unit`.
    * A `Quantity` describes the physical significance of a `Unit`.

A `Value` is created by applying a `Unit` to a `Double` using the `(*|)` operator,
which is read as "multiply in units."

Provides automatic unit conversion:

> > print $ numValue (1 *| h + 1 *| min') s        -- Time in seconds of 1 hour + 1 minute.
> 3660.0

Automatic unit reduction:

> > print $ numValue (20 *| gpm * 10 *| min') gal  -- Note the minutes cancel each other out.
> 200.0

And consistency checking:

> > print $ numValue (22 *| mph + 3 *| gal) mph      -- Note that speed (m/s) is inconsistent with volume (m^3).
> *** Exception: Incompatible units: [M]/[S] /= [M,M,M]/[]

Infix function `/|` is the same as `numValue`.  The symbol is read as "divide out units."

> > print $ 22 *| gpm * 10 *| min' /| gal

And defining new units is easy:

> -- | Millimeters, a measure of distance, is 1/1000 of a meter.
> mm :: Unit
> mm = 0.001 * m
> -- | Alternatively, use an SI prefix or SI prefix symbol
> mm = milli m
> mm = _m m

> -- | Joules, a measure of energy, is one newton meter.
> j :: Unit
> j = n * m

The `Quantity` of a `Value` or a `Unit` can be inspected.

> > quantity j
> Energy

> > quantity $ 10 *| m / 2 *| s
> Velocity

-}
{-# LANGUAGE EmptyDataDecls #-}
module Data.EngineeringUnits
  (
  -- * Fundamental Types and Operations
    Value
  , Unit
  , numValue
  , (*|)
  , (/|)
  , Quantity
  , quantity

  -- * SI Prefix Names <<http://physics.nist.gov/cuu/Units/prefixes.html (NIST listing)>>
  , yocto
  , zepto
  , atto
  , femto
  , pico
  , nano
  , micro
  , milli
  , centi
  , deci
  , deka
  , hecto
  , kilo
  , mega
  , giga
  , tera
  , peta
  , exa
  , zetta
  , yotta

  -- * SI Prefix Symbols <<http://physics.nist.gov/cuu/Units/prefixes.html (NIST listing)>>
  , _y
  , _z
  , _a
  , _f
  , _p
  , _n
  , _u
  , _m
  , _c
  , _d
  , _da
  , _h
  , _k
  , _M
  , _G
  , _T
  , _P
  , _E
  , _Z
  , _Y

  -- * Distance
  , m
  , cm
  , mm
  , km
  , in'
  , ft
  , mi
  -- * Area
  , m2
  , cm2
  , in2
  -- * Volume
  , cm3
  , ml
  , l
  , in3
  , gal
  -- * Mass
  , kg
  , g
  , mg
  -- * Force
  , n
  , lbs
  -- * Rotation
  , rev
  -- * Speed
  , mph
  , kph
  -- * Rotational Rate
  , rpm
  -- * Time
  , s
  , min'
  , h
  -- * Energy
  , j
  , btu
  -- * Power
  , hp
  , w
  , kw
  -- * Pressure
  , psi
  , bar
  -- * Flow
  , gpm
  , lpm
  -- * Misc
  , s2
  , s3
  , radsPerRev
  ) where

import           Data.List

-- | The base units for distance, time, mass, and revolutions.
data BaseUnit = Kg | M | Rev | S deriving (Eq, Ord, Show)

-- | A value is a number and its associated units.  It can be either a Unit
-- or a Value, depending on the phantom type.
data Dim a = Dim Double [BaseUnit] [BaseUnit] deriving (Show, Eq, Ord)
data DUnit
data DValue
type Unit = Dim DUnit
type Value = Dim DValue

instance Num (Dim a) where
  a@(Dim aV _ _) + b@(Dim bV _ _) = same a b $ aV + bV
  a@(Dim aV _ _) - b@(Dim bV _ _) = same a b $ aV - bV
  Dim aV aN aD * Dim bV bN bD = normalize $ Dim (aV * bV) (aN ++ bN) (aD ++ bD)
  fromInteger a = Dim (fromIntegral a) [] []
  negate (Dim v n d) = Dim (negate v) n d
  abs    (Dim v n d) = Dim (abs    v) n d
  signum (Dim v n d) = Dim (signum v) n d

instance Fractional (Dim a) where
  Dim aV aN aD / Dim bV bN bD = normalize $ Dim (aV / bV) (aN ++ bD) (aD ++ bN)
  recip (Dim v n d) = Dim (recip v) d n
  fromRational a = Dim (fromRational a) [] []

instance Floating (Dim a) where
  pi    = Dim pi [] []
  exp   = error "Not supported yet for Dim: exp  "
  log   = error "Not supported yet for Dim: log  "
  sin   = error "Not supported yet for Dim: sin  "
  cos   = error "Not supported yet for Dim: cos  "
  sinh  = error "Not supported yet for Dim: sinh "
  cosh  = error "Not supported yet for Dim: cosh "
  asin  = error "Not supported yet for Dim: asin "
  acos  = error "Not supported yet for Dim: acos "
  atan  = error "Not supported yet for Dim: atan "
  asinh = error "Not supported yet for Dim: asinh"
  acosh = error "Not supported yet for Dim: acosh"
  atanh = error "Not supported yet for Dim: atanh"

-- | Normalize a value, i.e. simplify and sort units.
normalize :: Dim a -> Dim a
normalize a@(Dim _ n d) = order $ reduce (n ++ d) a
  where
  reduce :: [BaseUnit] -> Dim a -> Dim a
  reduce [] a = a
  reduce (a : rest) (Dim v n d)
    | elem a n && elem a d = reduce rest $ Dim v (delete a n) (delete a d)
    | otherwise            = reduce rest $ Dim v n d
  order :: Dim a -> Dim a
  order (Dim v n d) = Dim v (sort n) (sort d)

-- | Create a value if two units are compatible.
same :: Dim a -> Dim b -> Double -> Dim a
same (Dim _ aN aD) (Dim _ bN bD) v
  | aN == bN && aD == bD = Dim v aN aD
  | otherwise = error $ "Incompatible units: " ++ show aN ++ "/" ++ show aD ++ " /= " ++ show bN ++ "/" ++ show bD

-- | Extract the numeric value of a Value in the given Unit.  An error
-- results if the Quanity and Unit are not dimensionally consistent.
--
--   > numValue quant units
--   > numValue (2.54 [ cm) in'
numValue :: Value -> Unit -> Double
numValue val@(Dim v _ _) units@(Dim k _ _) = result
  where
  Dim result _ _ = same val units $ v / k

-- | Multiply a numeric value by a Unit to make a Value.
(*|) :: Double -> Unit -> Value
num *| (Dim m n d) = Dim (num*m) n d

-- | Infix form of numValue. Divide a `Value` by a `Unit` to turn it into a
-- numeric value.
(/|) :: Value -> Unit -> Double
q /| u = numValue q u
infixl 1 /|

-- | Type describing the significance of a calculated `Value` or `Unit`.
data Quantity = Length | Time | Mass | Rotation
              | Area | Volume | Velocity | Acceleration
              | Wavenumber | Density | SpecificVolume
              | Energy | Power | Momentum | Force | Frequency
              | Other
              deriving (Eq, Ord, Show)

-- | Inspect a `Value` or `Unit` to determine its `Quantity`.
quantity :: Dim a -> Quantity
quantity (Dim _ [M]       [])       = Length
quantity (Dim _ [Kg]      [])       = Mass
quantity (Dim _ [S]       [])       = Time
quantity (Dim _ [Rev]     [])       = Rotation
quantity (Dim _ [M,M]     [])       = Area
quantity (Dim _ [M,M,M]   [])       = Volume
quantity (Dim _ [M]       [S])      = Velocity
quantity (Dim _ [M]       [S,S])    = Acceleration
quantity (Dim _ []        [M])      = Wavenumber
quantity (Dim _ [Kg]      [M,M,M])  = Density
quantity (Dim _ [M,M,M]   [Kg])     = SpecificVolume
quantity (Dim _ [Kg,M,M]  [S,S])    = Energy
quantity (Dim _ [Kg,M,M]  [S,S,S])  = Power
quantity (Dim _ [Kg,M]    [S])      = Momentum
quantity (Dim _ [Kg,M]    [S,S])    = Force
quantity (Dim _ []        [S])      = Frequency
quantity (Dim{})                    = Other

-- | Tranform a `Unit` into a multiple of another `Unit`.
--
-- > kilo = prefix 1e3
-- > km = kilo m
prefix :: Double -> Unit -> Unit
prefix a = (*) (Dim a [] [])

yocto = prefix 1e-24
zepto = prefix 1e-21
atto  = prefix 1e-18
femto = prefix 1e-15
pico  = prefix 1e-12
nano  = prefix 1e-9
micro = prefix 1e-6
milli = prefix 1e-3
centi = prefix 1e-2
deci  = prefix 1e-1
deka  = prefix 1e+1
hecto = prefix 1e+2
kilo  = prefix 1e+3
mega  = prefix 1e+6
giga  = prefix 1e+9
tera  = prefix 1e+12
peta  = prefix 1e+15
exa   = prefix 1e+18
zetta = prefix 1e+21
yotta = prefix 1e+24

_y  = yocto
_z  = zepto
_a  = atto
_f  = femto
_p  = pico
_n  = nano
_u  = micro
_m  = milli
_c  = centi
_d  = deci
_da = deka
_h  = hecto
_k  = kilo
_M  = mega
_G  = giga
_T  = tera
_P  = peta
_E  = exa
_Z  = zetta
_Y  = yotta


-- | Meters.
m    = Dim 1 [M]   [] :: Unit
-- | Meters ^ 2.
m2   = m * m
-- | Seconds.
s    = Dim 1 [S]   [] :: Unit
-- | Kilograms.
kg   = Dim 1 [Kg]  [] :: Unit
-- | Revolutions.
rev  = Dim 1 [Rev] [] :: Unit
-- | Seconds ^ 2.
s2   = s * s
-- | Seconds ^ 3.
s3   = s * s * s
-- | Centimeters.
cm   = _c m
-- | Centimeters ^ 2.
cm2  = cm * cm
-- | Centimeters ^ 3.
cm3  = cm * cm * cm
-- | Millimeters.
mm   = _m m
-- | Kilometers.
km   = _k m
-- | Milliliters.
ml   = cm3
-- | Liters.
l    = 1000 * ml
-- | Grams.
g    = 0.001 * kg
-- | Milligrams.
mg   = _m g
-- | Inches.
in'  = 2.54 * cm
-- | Inches ^ 2.
in2  = in' * in'
-- | Inches ^ 3.
in3  = in' * in' * in'
-- | Feet.
ft   = 12 * in'
-- | Minutes.
min' = 60 * s
-- | Hours.
h    = 60 * min'
-- | Newtons.
n    = kg * m / s2
-- | Pounds.
lbs  = 4.4482216152605 * n
-- | Miles.
mi   = 5280 * ft
-- | Gallons.
gal  = 231 * in3
-- | Horsepower.
hp   = 33000 * ft * lbs / min'
-- | Kilowatts.
kw   = _k w
-- | Watts.
w    = kg * m2 / s3
-- | Pounds per inch ^ 2.
psi  = lbs / in2
-- | Bar.
bar  = 14.5037738 * psi
-- | Miles per hour.
mph  = mi / h
-- | Kilometers per hour.
kph  = km / h
-- | Revolutions per minute.
rpm  = rev / min'
-- | Gallons per minute.
gpm  = gal / min'
-- | Liters per minute.
lpm  = l / min'
-- | Radians per revolution: 2 * pi / (1 *| rev)
radsPerRev = 2 * pi / (1 *| rev)
-- | Joules.
j    = n * m
-- | BTUs.
btu  = 1055.05585 * j


