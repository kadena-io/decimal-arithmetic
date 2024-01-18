{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module Numeric.Decimal.Precision
       ( Precision(..)
       , FinitePrecision(..)

       , P1 , P2 , P3 , P4 , P5 , P6 , P7 , P8 , P9 , P10
       , P11, P12, P13, P14, P15, P16, P17, P18, P19, P20
       , P21, P22, P23, P24, P25, P26, P27, P28, P29, P30
       , P31, P32, P33, P34, P35, P36, P37, P38, P39, P40
       , P41, P42, P43, P44, P45, P46, P47, P48, P49, P50

       , P75, P100, P150, P200, P250, P300, P400, P500, P1000, P2000

       , PPlus1, PTimes2

       , PInfinite
       ) where

import {-# SOURCE #-} Numeric.Decimal.Number
import Data.Proxy
import GHC.TypeLits
import qualified GHC.TypeLits as TL

data Prec (p :: Nat)

-- | Precision indicates the maximum number of significant decimal digits a
-- number may have.
class Precision p where
  -- | Return the precision of the argument, or 'Nothing' if the precision is
  -- infinite.
  precision :: Proxy p -> Maybe Int

  -- | Return the maximum exponent for a number in scientific notation with
  -- the given precision, or 'Nothing' if the exponent has no limit.
  eMax :: Proxy p -> Maybe Exponent
  eMax n = subtract 1 . (10 ^) . numDigits <$> base
    where mlength = precision (Proxy :: Proxy p)  :: Maybe Int
          base    = (10 *) . fromIntegral <$> mlength :: Maybe Coefficient

  -- | Return the minimum exponent for a number in scientific notation with
  -- the given precision, or 'Nothing' if the exponent has no limit.
  eMin :: Proxy p -> Maybe Exponent
  eMin = fmap (1 -) . eMax

-- | A subclass of precisions that are finite
class Precision p => FinitePrecision p where
  finitePrecision :: Proxy p -> Int

-- | A precision of unlimited significant digits
data PInfinite
instance Precision PInfinite where
  precision _ = Nothing
  eMax      _ = Nothing

-- | A precision of 1 significant digit
type P1 = Prec 1
-- data P1
instance KnownNat p => Precision (Prec p) where
  precision p = Just (fromIntegral (natVal (Proxy @p)))
  {-# INLINE precision #-}

instance KnownNat p => FinitePrecision (Prec p) where
  finitePrecision _ = fromIntegral (natVal (Proxy @p))

-- | A precision of (@p@ + 1) significant digits
-- type PPlus1 p = Prec (p TL.+ 1)
data PPlus1 p
instance Precision p => Precision (PPlus1 p) where
  precision pp = (+ 1) <$> precision (Proxy :: Proxy p)
instance FinitePrecision p => FinitePrecision (PPlus1 p) where
  finitePrecision _ = finitePrecision (Proxy @p) + 1

-- type PTimes2 p = Prec (p TL.* 2)
-- | A precision of (@p@ × 2) significant digits
data PTimes2 p
instance Precision p => Precision (PTimes2 p) where
  precision pp = (* 2) <$> precision (Proxy @p)
    -- where div2 :: PTimes2 p -> p
    --       div2 = undefined
instance FinitePrecision p => FinitePrecision (PTimes2 p) where
  finitePrecision _ = finitePrecision (Proxy @p) * 2

-- | A precision of 2 significant digits
type P2  = Prec 2 ; type P3  = Prec 3
-- ^ A precision of 3 significant digits

-- | Et cetera
type P4  = Prec 4 ; type P5  = Prec 5
type P6  = Prec 6 ; type P7  = Prec 7
type P8  = Prec 8 ; type P9  = Prec 9
type P10 = Prec 10 ; type P11 = Prec 11
type P12 = Prec 12 ; type P13 = Prec 13
type P14 = Prec 14 ; type P15 = Prec 15
type P16 = Prec 16 ; type P17 = Prec 17
type P18 = Prec 18 ; type P19 = Prec 19
type P20 = Prec 20; type P21 = Prec 21
type P22 = Prec 22; type P23 = Prec 23
type P24 = Prec 24; type P25 = Prec 25
type P26 = Prec 26; type P27 = Prec 27
type P28 = Prec 28; type P29 = Prec 29
type P30 = Prec 30; type P31 = Prec 31
type P32 = Prec 32; type P33 = Prec 33
type P34 = Prec 34; type P35 = Prec 35
type P36 = Prec 36; type P37 = Prec 37
type P38 = Prec 38; type P39 = Prec 39
type P40 = Prec 40; type P41 = Prec 41
type P42 = Prec 42; type P43 = Prec 43
type P44 = Prec 44; type P45 = Prec 45
type P46 = Prec 46; type P47 = Prec 47
type P48 = Prec 48; type P49 = Prec 49

type P50 = Prec 50
type P62 = Prec 62
type P74 = Prec 74; type P75 = Prec 75

type P100 = Prec 100
type P124 = Prec 124; type P125 = Prec 125
type P150 = Prec 150

type P200 = Prec 200
type P250 = Prec 250

type P300 = Prec 300
type P400 = Prec 400
type P500 = Prec 500

type P1000 = Prec 1000
type P2000 = Prec 2000
