{-# LANGUAGE OverloadedStrings, BangPatterns, NumericUnderscores, DeriveGeneric, TypeApplications #-}
module Data.Time.Duration
    ( daysPerCentury
    , nanoSecondsPerMicrosecond
    , nanoSecondsPerMillisecond
    , nanoSecondsPerSecond
    , nanoSecondsPerSecondW32
    , nanoSecondsPerMinute
    , nanoSecondsPerHour
    , nanoSecondsPerDay
    , nanoSecondsPerCentury
    , Duration
    , zero
    , minDur
    , maxDur
    , j2000ToJ1900Duration
    , epsilon
    , minPositive
    , minNegative
    , isNegative
    , new
    , fromParts
    , fromTotalNanoseconds
    , fromTruncatedNanoseconds
    , normalize
    ) where

import           RIO


daysPerCentury :: Word64
daysPerCentury = 36_525

nanoSecondsPerMicrosecond :: Word64
nanoSecondsPerMicrosecond = 1_000

nanoSecondsPerMillisecond :: Word64
nanoSecondsPerMillisecond = 1_000 * nanoSecondsPerMicrosecond

nanoSecondsPerSecond :: Word64
nanoSecondsPerSecond = 1_000 * nanoSecondsPerMillisecond

nanoSecondsPerSecondW32 :: Word32
nanoSecondsPerSecondW32 = 1_000_000_000

nanoSecondsPerMinute :: Word64
nanoSecondsPerMinute = 60 * nanoSecondsPerSecond

nanoSecondsPerHour :: Word64
nanoSecondsPerHour = 60 * nanoSecondsPerMinute

nanoSecondsPerDay :: Word64
nanoSecondsPerDay = 24 * nanoSecondsPerHour

nanoSecondsPerCentury :: Word64
nanoSecondsPerCentury = daysPerCentury * nanoSecondsPerDay



-- | Defines generally usable durations for nanosecond precision valid for 32,768 centuries in either direction, and only on 80 bits / 10 octets.
--
-- **Important conventions:**
-- Conventions had to be made to define the partial order of a duration.
-- 1. It was decided that the nanoseconds corresponds to the nanoseconds _into_ the current century. In other words,
-- a durationn with centuries = -1 and nanoseconds = 0 is _a smaller duration_ than centuries = -1 and nanoseconds = 1.
-- That difference is exactly 1 nanoseconds, where the former duration is "closer to zero" than the latter.
-- As such, the largest negative duration that can be represented sets the centuries to i16::MAX and its nanoseconds to NANOSECONDS_PER_CENTURY.
-- 2. It was also decided that opposite durations are equal, e.g. -15 minutes == 15 minutes. If the direction of time matters, use the signum function.
data Duration = Duration
    { centuries   :: !Int16
    , nanoseconds :: !Word64
    }
    deriving (Ord, Show, Read, Generic)


instance Eq Duration where
    d1 == d2
        | centuries d1 == centuries d2
        = nanoseconds d1 == nanoseconds d2
        | (abs (centuries d1 - centuries d2) == 1)
            && (centuries d1 == 0 || centuries d2 == 0)
        = if centuries d1 < 0
            then (nanoSecondsPerCentury - nanoseconds d1) == nanoseconds d2
            else (nanoSecondsPerCentury - nanoseconds d2) == nanoseconds d1
        | otherwise
        = False


-- | Returns whether this is a negative or positive duration.
{-# INLINABLE isNegative #-}
isNegative :: Duration -> Bool
isNegative d = centuries d < 0


-- | A duration of exactly zero nanoseconds
{-# INLINABLE zero #-}
zero :: Duration
zero = Duration 0 0

-- | Maximum duration that can be represented
{-# INLINABLE maxDur #-}
maxDur :: Duration
maxDur = Duration maxBound nanoSecondsPerCentury

-- | Minimum duration that can be represented
{-# INLINABLE minDur #-}
minDur :: Duration
minDur = Duration minBound nanoSecondsPerCentury


-- | Smallest duration that can be represented
{-# INLINABLE epsilon #-}
epsilon :: Duration
epsilon = Duration 0 1

-- | Minimum positive duration is one nanoseconds
{-# INLINABLE minPositive #-}
minPositive :: Duration
minPositive = epsilon

-- | Minimum negative duration is minus one nanosecond
{-# INLINABLE minNegative #-}
minNegative :: Duration
minNegative = Duration (-1) (nanoSecondsPerCentury - 1)


-- | The duration between J2000 and J1900: one century **minus** twelve hours. J1900 starts at  _noon_ but J2000 is at midnight.
{-# INLINABLE j2000ToJ1900Duration #-}
j2000ToJ1900Duration :: Duration
j2000ToJ1900Duration = Duration 0 3155716800000000000

-- | Builds a new duration from the number of centuries and the number of nanoseconds
{-# INLINABLE new #-}
new :: Int16 -> Word64 -> Duration
new c n = normalize (Duration c n)


-- | Create a normalized duration from its parts
{-# INLINABLE fromParts #-}
fromParts :: Int16 -> Word64 -> Duration
fromParts c n = normalize (Duration c n)


-- | Converts the total nanoseconds as a Natural into this Duration 
{-# INLINABLE fromTotalNanoseconds #-}
fromTotalNanoseconds :: Natural -> Duration
fromTotalNanoseconds nanos = if nanos == 0
    then zero
    else
        let (cents, remainingNanos) =
                nanos `quotRem` fromIntegral nanoSecondsPerCentury
        in  if cents > fromIntegral (maxBound @Int16)
                then maxDur
                else if cents < fromIntegral (minBound @Int16)
                    then minDur
                    else fromParts (fromIntegral cents)
                                   (fromIntegral remainingNanos)


-- | Create a new duration from the truncated nanoseconds (+/- 2927.1 years of duration)
{-# INLINABLE fromTruncatedNanoseconds #-}
fromTruncatedNanoseconds :: Int64 -> Duration
fromTruncatedNanoseconds nanos = if nanos < 0
    then
        let ns                         = fromIntegral (abs nanos)
            (extraCenturies, remNanos) = ns `quotRem` nanoSecondsPerCentury
        in  if extraCenturies > maxBound
                then minDur
                else fromParts ((-1) - fromIntegral extraCenturies)
                               (nanoSecondsPerCentury - remNanos)
    else fromParts 0 (fromIntegral (abs nanos))



{-# INLINABLE normalize #-}
normalize :: Duration -> Duration
normalize d@(Duration cen nan) =
    let (extraCenturies, remNanos) = nan `quotRem` nanoSecondsPerCentury
    in
        if extraCenturies > 0
            then if (cen == minBound) && (remNanos > 0)
                then minDur
                else if (cen == maxBound) && (remNanos > 0)
                    then maxDur
                    else if cen >= 0
                        then if fromIntegral (maxBound - cen) >= extraCenturies
                            then
                                let newCen = cen + fromIntegral extraCenturies
                                in  Duration newCen remNanos
                            else maxDur
                        else if fromIntegral (minBound - cen) >= extraCenturies
                            then
                                let newCen = cen + fromIntegral extraCenturies
                                in  Duration newCen remNanos
                            else minDur
            else d
