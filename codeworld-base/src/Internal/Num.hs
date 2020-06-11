{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}
module Internal.Num
  ( Number,
    (+),
    (-),
    (*),
    (/),
    (^),
    (>),
    (>=),
    (<),
    (<=),
    max,
    min,
    negate,
    abs,
    absoluteValue,
    signum,
    truncation,
    rounded,
    ceiling,
    floor,
    quotient,
    remainder,
    pi,
    exp,
    sqrt,
    squareRoot,
    log,
    logBase,
    sin,
    tan,
    cos,
    asin,
    atan,
    acos,
    properFraction,
    even,
    odd,
    gcd,
    lcm,
    sum,
    product,
    maximum,
    minimum,
    isInteger,
    fromInteger,
    fromRational,
    fromInt,
    toInt,
    fromDouble,
    toDouble,
  )
where

import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Internal.Truth
import Internal.Truth ((&&), Truth, otherwise)
import Numeric (showFFloatAlt)
import qualified "base" Prelude as P
import "base" Prelude ((.), (==), Bool (..), map)

-- | The type for numbers.
--
--   Numbers can be positive or negative, whole or fractional.  For example, 5,
--   3.2, and -10 are all values of the type Number.
newtype Number
  = Number P.Double
  deriving (P.RealFrac, P.Real, P.Floating, P.RealFloat, P.Eq)

{-# RULES
"equality/num" forall (x :: Number).
  (Internal.Truth.==) x =
    (P.==) x
  #-}

{-# RULES
"equality/point" forall (x :: (Number, Number)).
  (Internal.Truth.==) x =
    (P.==) x
  #-}

fromDouble :: HasCallStack => P.Double -> Number
fromDouble x
  | P.isNaN x = P.error "Number is undefined."
  | P.isInfinite x = P.error "Number is too large."
  | otherwise = Number x

toDouble :: Number -> P.Double
toDouble (Number x) = x

fromInteger :: P.Integer -> Number
fromInteger = fromDouble . P.fromInteger

fromRational :: P.Rational -> Number
fromRational = fromDouble . P.fromRational

fromInt :: P.Int -> Number
fromInt = fromDouble . P.fromIntegral

toInt :: HasCallStack => Number -> P.Int
toInt n
  | isInteger n = P.truncate (toDouble n)
  | otherwise = P.error "Whole number is required."

instance P.Show Number where
  show (Number x) = stripZeros (showFFloatAlt (P.Just 4) x "")
    where
      stripZeros =
        P.reverse . P.dropWhile (== '.') . P.dropWhile (== '0') . P.reverse

instance P.Num Number where
  fromInteger = fromInteger
  (+) = (+)
  (-) = (-)
  (*) = (*)
  negate (Number n) = Number (P.negate n)
  abs = abs
  signum = signum

instance P.Fractional Number where
  fromRational x = Number (P.fromRational x)
  (/) = (/)

instance P.Enum Number where
  succ = fromDouble . P.succ . toDouble
  pred = fromDouble . P.pred . toDouble
  toEnum = fromDouble . P.toEnum
  fromEnum = P.fromEnum . toDouble
  enumFrom = map fromDouble . P.enumFrom . toDouble
  enumFromThen (Number a) (Number b) = map fromDouble (P.enumFromThen a b)
  enumFromTo (Number a) (Number b) = map fromDouble (P.enumFromTo a b)
  enumFromThenTo (Number a) (Number b) (Number c) =
    map fromDouble (P.enumFromThenTo a b c)

instance P.Ord Number where
  compare (Number a) (Number b) = P.compare a b

-- | Tells whether a Number is an integer or not.
--
--  An integer is a whole number, such as 5, 0, or -10.  Numbers with non-zero
--  decimals, like 5.3, are not integers.
isInteger :: Number -> Truth
isInteger (Number x) = x == P.fromIntegral (P.truncate x)

infixr 8 ^

infixl 7 *, /

infixl 6 +, -

infix 4 <, <=, >=, >

-- | Adds two numbers.
(+) :: Number -> Number -> Number
Number a + Number b = fromDouble (a P.+ b)

-- | Subtracts two numbers.
(-) :: Number -> Number -> Number
Number a - Number b = fromDouble (a P.- b)

-- | Multiplies two numbers.
(*) :: Number -> Number -> Number
Number a * Number b = fromDouble (a P.* b)

-- | Divides two numbers.  The second number should not be zero.
(/) :: HasCallStack => Number -> Number -> Number
Number a / Number b
  | b == 0 = withFrozenCallStack (P.error "Cannot divide by zero.")
  | otherwise = fromDouble (a P./ b)

-- | Raises a number to a power.
(^) :: HasCallStack => Number -> Number -> Number
Number a ^ Number b
  | a P.< 0 && P.not (isInteger (Number b)) =
    withFrozenCallStack
      (P.error "Negative numbers cannot be raised to fractional powers.")
  | a P.== 0 && b P.< 0 =
    withFrozenCallStack
      (P.error "Zero cannot be raised to negative powers.")
  | otherwise = fromDouble (a P.** b)

-- | Tells whether one number is less than the other.
(<) :: Number -> Number -> Truth
Number a < Number b = a P.< b

-- | Tells whether one number is less than or equal to the other.
(<=) :: Number -> Number -> Truth
Number a <= Number b = a P.<= b

-- | Tells whether one number is greater than the other.
(>) :: Number -> Number -> Truth
Number a > Number b = a P.> b

-- | Tells whether one number is greater than or equal to the other.
(>=) :: Number -> Number -> Truth
Number a >= Number b = a P.>= b

-- | Gives the larger of two numbers.
max :: (Number, Number) -> Number
max (Number a, Number b) = fromDouble (P.max a b)

-- | Gives the smaller of two numbers.
min :: (Number, Number) -> Number
min (Number a, Number b) = fromDouble (P.min a b)

negate :: Number -> Number
negate = P.negate

-- | Gives the absolute value of a number.
--
--  If the number if positive or zero, the absolute value is the same as the
--  number.  If the number is negative, the absolute value is the opposite of
--  the number.
abs :: Number -> Number
abs = fromDouble . P.abs . toDouble

absoluteValue :: Number -> Number
absoluteValue = abs

-- | Gives the sign of a number.
--
--  If the number is negative, the signum is -1.  If it's positive, the signum
--  is 1.  If the number is 0, the signum is 0.  In general, a number is equal
--  to its absolute value ('abs') times its sign ('signum').
signum :: Number -> Number
signum = fromDouble . P.signum . toDouble

-- | Gives the number without its fractional part.
--
--  For example, truncate(4.2) is 4, while truncate(-4.7) is -4.
truncation :: Number -> Number
truncation = fromInteger . P.truncate . toDouble

-- | Gives the number rounded to the nearest integer.
--
--  For example, round(4.2) is 4, while round(4.7) is 5.
rounded :: Number -> Number
rounded = fromInteger . P.round . toDouble

-- | Gives the smallest integer that is greater than or equal to a number.
--
--  For example, ceiling(4) is 4, while ceiling(4.1) is 5.  With negative
--  numbers, ceiling(-3.5) is -3, since -3 is greater than -3.5.
ceiling :: Number -> Number
ceiling = fromInteger . P.ceiling . toDouble

-- | Gives the largest integer that is less than or equal to a number.
--
--  For example, floor(4) is 4, while floor(3.9) is 3.  With negative
--  numbers, floor(-3.5) is -4, since -4 is less than -3.5.
floor :: Number -> Number
floor = fromInteger . P.floor . toDouble

-- | Gives the integer part of the result when dividing two numbers.
--
--  For example, 3/2 is 1.5, but quotient(3, 2) is 1, which is the integer
--  part.
quotient :: HasCallStack => (Number, Number) -> Number
quotient (_, 0) = withFrozenCallStack (P.error "Cannot divide by zero.")
quotient (a, b) = truncation (a / b)

-- | Gives the remainder when dividing two numbers.
--
--  For example, remainder(3,2) is 1, which is the remainder when dividing
--  3 by 2.
remainder :: HasCallStack => (Number, Number) -> Number
remainder (a, 0) = withFrozenCallStack (P.error "Cannot divide by zero.")
remainder (a, b) = a - b * quotient (a, b)

-- | The constant pi, which is equal to the ratio between the circumference
--    and diameter of a circle.
--
--  pi is approximately 3.14.
pi :: Number
pi = fromDouble 3.141592653589793

-- | Gives the exponential of a number.  This is equal to the constant e,
--    raised to the power of the number.
--
--  The exp function increases faster and faster very quickly.  For example,
--  if t is the current time in seconds, exp(t) will reach a million in about
--  14 seconds.  It will reach a billion in around 21 seconds.
exp :: Number -> Number
exp = fromDouble . P.exp . toDouble

-- | Gives the square root of a number.  This is the positive number that, when
--    multiplied by itself, gives the original number back.
--
--  The sqrt always increases, but slows down.  For example, if t is the
--  current time, sqrt(t) will reach 5 in 25 seconds.  But it will take 100
--  seconds to reach 10, and 225 seconds (almost 4 minutes) to reach 15.
sqrt :: HasCallStack => Number -> Number
sqrt (Number x)
  | x P.< 0 =
    withFrozenCallStack (P.error "Negative numbers have no square root.")
  | otherwise = fromDouble (P.sqrt x)

squareRoot :: HasCallStack => Number -> Number
squareRoot = sqrt

-- | Gives the natural log of a number.  This is the opposite of the exp
--    function.
--
--  Like sqrt, the log function always increases, but slows down.  However,
--  it slows down much sooner than the sqrt function.  If t is the current time
--  in seconds, it takes more than 2 minutes for log(t) to reach 5, and more
--  than 6 hours to reach 10!
log :: HasCallStack => Number -> Number
log (Number x)
  | x P.<= 0 =
    withFrozenCallStack (P.error "Only positive numbers have logarithms.")
  | otherwise = fromDouble (P.log x)

-- | Gives the logarithm of the first number, using the base of the second
--    number.
logBase :: HasCallStack => (Number, Number) -> Number
logBase (Number x, Number b)
  | x P.<= 0 =
    withFrozenCallStack (P.error "Only positive numbers have logarithms.")
  | b P.<= 0 =
    withFrozenCallStack
      (P.error "The base of a logarithm must be a positive number.")
  | b P.== 1 =
    withFrozenCallStack (P.error "A logarithm cannot have a base of 1.")
  | otherwise = fromDouble (P.logBase b x)

-- | Converts an angle from degrees to radians.
toRadians :: Number -> Number
toRadians d = d / 180 * pi

-- | Converts an angle from radians to degrees.
fromRadians :: Number -> Number
fromRadians r = r / pi * 180

-- | Gives the sine of an angle, where the angle is measured in degrees.
sin :: Number -> Number
sin = fromDouble . P.sin . toDouble . toRadians

-- | Gives the tangent of an angle, where the angle is measured in degrees.
--
--  This is the slope of a line at that angle from horizontal.
tan :: Number -> Number
tan = fromDouble . P.tan . toDouble . toRadians

-- | Gives the cosine of an angle, where the angle is measured in degrees.
cos :: Number -> Number
cos = fromDouble . P.cos . toDouble . toRadians

-- | Gives the inverse sine of a value, in degrees.
--
--  This is the unique angle between -90 and 90 that has the input as its sine.
asin :: HasCallStack => Number -> Number
asin (Number x)
  | x P.< -1 P.|| x P.> 1 =
    withFrozenCallStack
      ( P.error
          "The asin function is only defined for numbers from -1 to 1."
      )
  | otherwise = fromRadians (fromDouble (P.asin x))

-- | Gives the inverse cosine of a value, in degrees.
--
--  This is the unique angle between 0 and 180 that has the input as its cosine.
acos :: HasCallStack => Number -> Number
acos (Number x)
  | x P.< -1 P.|| x P.> 1 =
    withFrozenCallStack
      ( P.error
          "The acos function is only defined for numbers from -1 to 1."
      )
  | otherwise = fromRadians (fromDouble (P.acos x))

-- | Gives the inverse tangent of a value, in degrees.
--
--  This is the unique angle between -90 and 90 that has the input as its tangent.
atan :: Number -> Number
atan = fromRadians . fromDouble . P.atan . toDouble

-- | Separates a number into its whole and fractional parts.
--
--  For example, properFraction(1.2) is (1, 0.2).
properFraction :: Number -> (Number, Number)
properFraction (Number x) = (fromInteger w, fromDouble p)
  where
    (w, p) = P.properFraction x

-- | Tells if a number is even.
even :: Number -> Truth
even n
  | isInteger n = P.even (toInt n)
  | otherwise = False

-- | Tells if a number is odd.
odd :: Number -> Truth
odd n
  | isInteger n = P.odd (toInt n)
  | otherwise = False

-- | Gives the greatest common divisor of two numbers.
--
--  This is the largest number that divides each of the two parameters.
--  Both parameters must be integers.
gcd :: HasCallStack => (Number, Number) -> Number
gcd (a, b) = withFrozenCallStack (fromInt (P.gcd (toInt a) (toInt b)))

-- | Gives the least common multiple of two numbers.
--
--  This is the smallest number that is divisible by both of the two
--  parameters.  Both parameters must be integers.
lcm :: HasCallStack => (Number, Number) -> Number
lcm (a, b) = withFrozenCallStack (fromInt (P.lcm (toInt a) (toInt b)))

-- | Gives the sum of a list of numbers.
sum :: [Number] -> Number
sum = fromDouble . P.sum . P.map toDouble

-- | Gives the product of a list of numbers.
product :: [Number] -> Number
product = fromDouble . P.product . P.map toDouble

-- | Gives the largest number from a list.
maximum :: [Number] -> Number
maximum = fromDouble . P.maximum . P.map toDouble

-- | Gives the smallest number from a list.
minimum :: [Number] -> Number
minimum = fromDouble . P.minimum . P.map toDouble
