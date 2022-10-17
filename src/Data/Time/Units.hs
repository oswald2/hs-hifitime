{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}
module Data.Time.Units
    ( Unit(..)
    , inSeconds
    , fromSeconds
    , toU8
    , fromU8
    ) where

import           GHC.Generics
import           RIO

import           Data.Time.Constants


-- | An Enum to perform time unit conversions.
data Unit =
    Nanosecond
    | Microsecond
    | Millisecond
    | Second
    | Minute
    | Hour
    | Day
    -- 36525 days, it the number of days per century in the Julian calendar
    | Century
    deriving (Eq, Ord, Enum, Read, Show, Generic)


{-# INLINABLE inSeconds #-}
inSeconds :: Unit -> Double
inSeconds Century     = cDAYS_PER_CENTURY * cSECONDS_PER_DAY
inSeconds Day         = cSECONDS_PER_DAY
inSeconds Hour        = cSECONDS_PER_HOUR
inSeconds Minute      = cSECONDS_PER_MINUTE
inSeconds Second      = 1.0
inSeconds Millisecond = 1e-3
inSeconds Microsecond = 1e-6
inSeconds Nanosecond  = 1e-9


{-# INLINABLE fromSeconds #-}
fromSeconds :: Unit -> Double
fromSeconds u = 1.0 / inSeconds u

-- | Allows conversion of a u8 into a Unit. Defaults to Second if the u8 is not a valid Unit representation.
{-# INLINABLE toU8 #-}
toU8 :: Unit -> Word8
toU8 Nanosecond  = 1
toU8 Microsecond = 2
toU8 Millisecond = 3
toU8 Minute      = 4
toU8 Hour        = 5
toU8 Day         = 6
toU8 Century     = 7
toU8 Second      = 0


{-# INLINABLE fromU8 #-}
fromU8 :: Word8 -> Unit
fromU8 1 = Nanosecond
fromU8 2 = Microsecond
fromU8 3 = Millisecond
fromU8 4 = Minute
fromU8 5 = Hour
fromU8 6 = Day
fromU8 7 = Century
fromU8 _ = Second




-- | An Enum to convert frequencies to their approximate duration, **rounded to the closest nanosecond**.
data Freq =
    GigaHertz
    | MegaHertz
    | KiloHertz
    | Hertz
    deriving(Eq, Ord, Enum, Read, Show, Generic)



