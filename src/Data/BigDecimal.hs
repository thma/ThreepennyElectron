{-# LANGUAGE InstanceSigs #-}
{- | This module defines the type 'BigDecimal' which provides a representation of arbitrary precision decimal numbers.
     'BigDecimal' is a native Haskell implementation based on arbitrary sized 'Integer' values.
     The implementation was inspired by Java BigDecimals.

      BigDecimal instantiates the typeclasses 'Num', 'Fractional' and 'Real'. It is thus possible to use all common
          operators like '+', '-', '*', '/', '^' on them.

       Here are a few examples from an interactive GHCI session:

      >  λ> a = BigDecimal 144 2
      >  λ> toString a
      >  "1.44"
      >  λ> b = sqrt a
      >  λ> toString b
      >  "1.2"
      >  λ> b * b
      >  BigDecimal 144 2
      >  λ> b * b * b
      >  BigDecimal 1728 3
      >  λ> b^2
      >  BigDecimal 144 2
      >  λ> c = fromString "123.4567890"
      >  λ> c
      >  BigDecimal 1234567890 7
      >  λ> a / c
      >  BigDecimal 1166400010614240096589584878965222398584 41
      >  λ> roundBD it (halfUp 10)
      >  BigDecimal 116640001 10
      >  λ> divide (a, c) $ halfUp 20
      >  BigDecimal 1166400010614240097 20

-}
module Data.BigDecimal
  ( BigDecimal (..)
  , RoundingMode (..)
  , MathContext
  , getScale
  , getValue
  , precision
  , trim
  , nf
  , divide
  , roundBD
  , fromRatio
  , halfUp
  , fromString
  , matchScales
  , toString
  )
where

import           Data.List  (find, elemIndex)
import           Data.Maybe (fromMaybe)
import           GHC.Real   ((%), Ratio ((:%)))

-- | RoundingMode defines how to handle loss of precision in divisions or explicit rounding.
data RoundingMode
  = UP        -- ^ Rounding mode to round away from zero.
  | DOWN      -- ^ Rounding mode to round towards zero.
  | CEILING   -- ^ Rounding mode to round towards positive infinity.
  | FLOOR     -- ^ Rounding mode to round towards negative infinity.
  | HALF_UP   -- ^ Rounding mode to round towards "nearest neighbor" unless both neighbors are equidistant, in which case round up.
  | HALF_DOWN -- ^ Rounding mode to round towards "nearest neighbor" unless both neighbors are equidistant, in which case round down.
  | HALF_EVEN -- ^ Rounding mode to round towards "nearest neighbor" unless both neighbors are equidistant, in which case, round towards the even neighbor.
  | PRECISE   -- ^ Rounding mode to assert that the requested operation has an exact result, hence no rounding is applied.

{-| BigDecimal is represented by an unscaled Integer value plus a second Integer value that defines the scale
      E.g.: (BigDecimal 1234 2) represents the decimal value 12.34.

-}
data BigDecimal =
  -- | creates a BigDecimal value from an unscaled 'Integer' value and a scale, given as a positive 'Integer'.
  --   Example: (BigDecimal 1234 2) creates the value 12.34
  BigDecimal Integer Integer
  deriving (Show, Read)

-- | gets the scale part of a BigDecimal
getScale :: BigDecimal -> Integer
getScale (BigDecimal _ s) = s

-- | get the unscaled value of a BigDecimal
getValue :: BigDecimal -> Integer
getValue (BigDecimal v _) = v

-- | A MathContext is interpreted by divisions and rounding operations to specify the expected loss of precision and the rounding behaviour.
--   MathContext is a pair of a 'RoundingMode' and a target precision of type 'Maybe' 'Integer'. The precision defines the number of digits after the decimal point.
--   If 'Nothing' is given as precision all decimal digits are to be preserved, that is precision is not limited.
type MathContext = (RoundingMode, Maybe Integer)

instance Num BigDecimal where
  a + b                   = plus (a, b)
  a * b                   = mul (a, b)
  abs (BigDecimal v s)    = BigDecimal (abs v) s
  signum (BigDecimal v _) = BigDecimal (signum v) 0
  fromInteger i           = BigDecimal i 0
  negate (BigDecimal v s) = BigDecimal (-v) s

instance Eq BigDecimal where
  a == b =
    let (BigDecimal valA _, BigDecimal valB _) = matchScales (a, b)
    in valA == valB

instance Fractional BigDecimal where
  -- default division rounds up and does not limit precision
  a / b = nf $ divide (matchScales (a, b)) (HALF_UP, Just 20)
  fromRational ratio@(x :% y) = fromRatio ratio (HALF_UP, Just 20)

-- | creates a BigDecimal from a 'Rational' value. 'MathContext' defines precision and rounding mode.
fromRatio :: Rational -> MathContext -> BigDecimal
fromRatio (x :% y) = divide (fromInteger x, fromInteger y)

instance Real BigDecimal where
  toRational (BigDecimal val scale) = toRational val * 10^^(-scale)

instance Ord BigDecimal where
  compare a b =
    let (BigDecimal valA _, BigDecimal valB _) = matchScales (a, b)
    in compare valA valB

instance RealFrac BigDecimal where
  properFraction bd = (proper, fraction) where
    (proper, fraction) =
      let rdm = (DOWN, Just 0)
          properBD = roundBD bd rdm
          BigDecimal properInt _ = properBD
          fractionBD = bd - properBD
       in (fromInteger properInt, fractionBD)


-- | add two BigDecimals
plus :: (BigDecimal, BigDecimal) -> BigDecimal
plus (a@(BigDecimal valA scaleA), b@(BigDecimal valB scaleB))
  | scaleA == scaleB = BigDecimal (valA + valB) scaleA
  | otherwise        = plus $ matchScales (a,b)

-- | multiply two BigDecimals
mul :: (BigDecimal, BigDecimal) -> BigDecimal
mul (BigDecimal valA scaleA, BigDecimal valB scaleB) = BigDecimal (valA * valB) (scaleA + scaleB)

-- | divide two BigDecimals and applies the 'MathContext' (i.e. a tuple of 'RoundingMode' and the specified precision) for rounding.
divide :: (BigDecimal, BigDecimal)  -- ^  the tuple of dividend and divisor. I.e. (dividend, divisor)
       -> MathContext               -- ^ 'MathContext' (i.e. a tuple of 'RoundingMode' and the specified precision) defines the rounding behaviour.
                                    --   if 'Nothing' if given as precision the maximum possible precision is used.
       -> BigDecimal                -- ^ the resulting BigDecimal
divide (a, b) (rMode, prefScale) =
  let (BigDecimal numA _, BigDecimal numB _) = matchScales (a, b)
      maxPrecision = fromMaybe (precision a + round (fromInteger (precision b) * 10 / 3)) prefScale
  in trim maxPrecision (BigDecimal (divUsing rMode (numA * (10 :: Integer) ^ maxPrecision) numB) maxPrecision)

-- | divide two correctly scaled Integers and apply the RoundingMode
divUsing :: RoundingMode -> Integer -> Integer -> Integer
divUsing rounding a b =
  let (quot, rem) = quotRem a b
      delta = (10 * abs rem `div` abs b) - 5
  in case rounding of
       PRECISE   -> if rem     == 0 then quot else error "non-terminating decimal expansion"
       UP        -> if abs rem  > 0 then quot +  signum quot else quot
       CEILING   -> if abs rem  > 0 &&   quot >= 0 then quot + 1 else quot
       HALF_UP   -> if delta   >= 0 then quot +  signum quot else quot
       HALF_DOWN -> if delta   <= 0 then quot else quot +  signum quot
       DOWN      -> quot
       FLOOR     -> if quot    >= 0 then quot else quot - 1
       HALF_EVEN
         | delta  > 0             -> quot + signum quot
         | delta == 0 && odd quot -> quot + signum quot
         | otherwise              -> quot

-- | round a BigDecimal to 'n' digits applying the 'MathContext' 'mc'
roundBD :: BigDecimal -> MathContext -> BigDecimal
roundBD bd@(BigDecimal val scale) mc@(rMode, Just n)
  | n < 0 || n >= scale = bd
  | otherwise           = BigDecimal (divUsing rMode val (10 ^ (scale-n))) n

-- | match the scales of a tuple of BigDecimals
matchScales :: (BigDecimal, BigDecimal) -> (BigDecimal, BigDecimal)
matchScales (a@(BigDecimal integerA scaleA), b@(BigDecimal integerB scaleB))
  | scaleA < scaleB =    (BigDecimal (integerA * 10 ^ (scaleB - scaleA)) scaleB, b)
  | scaleA > scaleB = (a, BigDecimal (integerB * 10 ^ (scaleA - scaleB)) scaleA)
  | otherwise       = (a, b)

-- | returns the number of digits of an Integer
precision :: BigDecimal -> Integer
precision 0                  = 1
precision (BigDecimal val _) = 1 + floor (logBase 10 $ abs $ fromInteger val)

-- | removes trailing zeros from a BigDecimals intValue by decreasing the scale
trim :: Integer -> BigDecimal -> BigDecimal
trim prefScale bd@(BigDecimal val scale) =
  let (v, r) = quotRem val 10
  in if r == 0 && 0 <= prefScale && prefScale < scale
       then trim prefScale $ BigDecimal v (scale - 1)
       else bd

-- | computes the normal form of a BigDecimal
nf :: BigDecimal -> BigDecimal
nf = trim 0

-- | read a BigDecimal from a human readable decimal notation.
--   e.g. @ fromString "3.14" @ yields 'BigDecimal 314 2'
fromString :: String -> BigDecimal
fromString s =
  let maybeIndex = elemIndex '.' s
      intValue   = read (filter (/= '.') s) :: Integer
  in case maybeIndex of
       Nothing -> BigDecimal intValue 0
       Just i  -> BigDecimal intValue $ toInteger (length s - i - 1)

-- | returns a readable String representation of a BigDecimal
--   e.g. @ toString (BigDecimal 314 2) @ yields "3.14"
toString :: BigDecimal -> String
toString bd@(BigDecimal intValue scale) =
  let s = show $ abs intValue
      filled =
        if fromInteger scale >= length s
          then replicate (1 + fromInteger scale - length s) '0' ++ s
          else s
      splitPos = length filled - fromInteger scale
      (ints, decimals) = splitAt splitPos filled
      sign = if intValue < 0 then "-" else ""
  in sign ++ if not (null decimals) then ints ++ "." ++ decimals else ints

-- | construct a 'MathContext' for rounding 'HALF_UP' with 'scale' decimal digits
halfUp :: Integer -> MathContext
halfUp scale = (HALF_UP, Just scale)