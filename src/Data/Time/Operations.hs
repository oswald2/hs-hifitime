{-# LANGUAGE  
    TypeApplications
    , MultiParamTypeClasses
    , FlexibleContexts
#-}
module Data.Time.Operations
    ( Mul(..)
    , Add(..)
    , TimeUnits(..)
    ) where

import           RIO

import           Data.Time.Duration
import           Data.Time.Units


class Mul a b where
    (#*#) :: a -> b -> Duration

instance Mul Unit Int64 where
    un #*# i = mulI64 un i

instance Mul Int64 Unit where
    i #*# un = mulI64 un i

instance Mul Unit Double where
    un #*# d = mulDouble un d

instance Mul Double Unit where
    d #*# un = mulDouble un d


class Add a where
    add :: a -> a -> Duration



-- instance Add Unit where
--     add u1 u2 = (u1 #*# 1 :: Int64) + (u2 #*# 1)


mulI64 :: Unit -> Int64 -> Duration
mulI64 unit q =
    let totalNs = case unit of
            Century     -> q * fromIntegral nanoSecondsPerCentury
            Day         -> q * fromIntegral nanoSecondsPerDay
            Hour        -> q * fromIntegral nanoSecondsPerHour
            Minute      -> q * fromIntegral nanoSecondsPerMinute
            Second      -> q * fromIntegral nanoSecondsPerSecond
            Millisecond -> q * fromIntegral nanoSecondsPerMillisecond
            Microsecond -> q * fromIntegral nanoSecondsPerMicrosecond
            Nanosecond  -> q
    in  if abs totalNs < (maxBound @Int64)
            then fromTruncatedNanoseconds totalNs
            else fromTotalNanoseconds (fromIntegral totalNs)


mulDouble :: Unit -> Double -> Duration
mulDouble unit q =
    let totalNs = case unit of
            Century     -> q * fromIntegral nanoSecondsPerCentury
            Day         -> q * fromIntegral nanoSecondsPerDay
            Hour        -> q * fromIntegral nanoSecondsPerHour
            Minute      -> q * fromIntegral nanoSecondsPerMinute
            Second      -> q * fromIntegral nanoSecondsPerSecond
            Millisecond -> q * fromIntegral nanoSecondsPerMillisecond
            Microsecond -> q * fromIntegral nanoSecondsPerMicrosecond
            Nanosecond  -> q
    in  if abs totalNs < fromIntegral (maxBound @Int64)
            then fromTruncatedNanoseconds (round totalNs)
            else fromTotalNanoseconds (round totalNs)


class TimeUnits a where
    centuries :: Mul Unit a => a -> Duration
    centuries x = Century #*# x

    days :: Mul Unit a => a -> Duration
    days x = Day #*# x

    hours :: Mul Unit a => a -> Duration
    hours x = Hour #*# x

    minutes :: Mul Unit a => a -> Duration
    minutes x = Minute #*# x

    seconds :: Mul Unit a => a -> Duration
    seconds x = Second #*# x

    milliseconds :: Mul Unit a => a -> Duration
    milliseconds x = Millisecond #*# x

    microseconds :: Mul Unit a => a -> Duration
    microseconds x = Microsecond #*# x

    nanoseconds :: Mul Unit a => a -> Duration
    nanoseconds x = Nanosecond #*# x


