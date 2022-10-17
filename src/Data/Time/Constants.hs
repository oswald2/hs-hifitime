{-# LANGUAGE BangPatterns, NumericUnderscores #-}
module Data.Time.Constants
    ( cJ1900_NAIF
    , cJ2000_NAIF
    , cJ1900_OFFSET
    , cJ2000_OFFSET
    , cET_EPOCH_S
    , cDAYS_GPS_TAI_OFFSET
    , cSECONDS_GPS_TAI_OFFSET_I64
    , cSECONDS_GPS_TAI_OFFSET
    , cSECONDS_PER_SIDERAL_YEAR
    , cSECONDS_PER_TROPICAL_YEAR
    , cSECONDS_PER_CENTURY
    , cSECONDS_PER_HOUR
    , cSECONDS_PER_MINUTE
    , cSECONDS_PER_DAY
    , cDAYS_PER_CENTURY
    , cDAYS_PER_CENTURY_I64
    , cDAYS_PER_YEAR_NLD
    , cDAYS_PER_YEAR
    , cJDE_OFFSET_SECONDS
    ) where

import           RIO



cJ1900_NAIF :: Double
cJ1900_NAIF = 2_415_020.0


cJ2000_NAIF :: Double
cJ2000_NAIF = 2_451_545.0

-- | `J1900_OFFSET` determines the offset in julian days between 01 Jan 1900 at midnight and the
-- Modified Julian Day at 17 November 1858.
-- NOTE: Julian days "start" at noon so that astronomical observations throughout the night
-- happen at the same Julian day. Note however that the Modified Julian Date (MJD) starts at
-- midnight, not noon, cf. <http://tycho.usno.navy.mil/mjd.html>.
cJ1900_OFFSET :: Double
cJ1900_OFFSET = 15_020.0

-- | `J2000_OFFSET` determines the offset in julian days between 01 Jan 2000 at **noon** and the
-- Modified Julian Day at 17 November 1858.
cJ2000_OFFSET :: Double
cJ2000_OFFSET = 51_544.5

-- | The Ephemeris Time epoch, in seconds
cET_EPOCH_S :: Int64
cET_EPOCH_S = 3_155_716_800

-- | Modified Julian Date in seconds as defined [here](http://tycho.usno.navy.mil/mjd.html). MJD epoch is Modified Julian Day at 17 November 1858 at midnight.
cMJD_OFFSET :: Double
cMJD_OFFSET = 2_400_000.5

-- | The JDE offset in days
cJDE_OFFSET_DAYS :: Double
cJDE_OFFSET_DAYS = cJ1900_OFFSET + cMJD_OFFSET

-- | The JDE offset in seconds
cJDE_OFFSET_SECONDS :: Double
cJDE_OFFSET_SECONDS = cJDE_OFFSET_DAYS * cSECONDS_PER_DAY

-- | `DAYS_PER_YEAR` corresponds to the number of days per year in the Julian calendar.
cDAYS_PER_YEAR :: Double
cDAYS_PER_YEAR = 365.25

-- | `DAYS_PER_YEAR_NLD` corresponds to the number of days per year **without leap days**.
cDAYS_PER_YEAR_NLD :: Double
cDAYS_PER_YEAR_NLD = 365.0

-- | `DAYS_PER_CENTURY` corresponds to the number of days per centuy in the Julian calendar.
cDAYS_PER_CENTURY :: Double
cDAYS_PER_CENTURY = 36525.0

cDAYS_PER_CENTURY_I64 :: Int64
cDAYS_PER_CENTURY_I64 = 36525

-- | `SECONDS_PER_MINUTE` defines the number of seconds per minute.
cSECONDS_PER_MINUTE :: Double
cSECONDS_PER_MINUTE = 60.0

-- | `SECONDS_PER_HOUR` defines the number of seconds per hour.
cSECONDS_PER_HOUR :: Double
cSECONDS_PER_HOUR = 3_600.0

-- | `SECONDS_PER_DAY` defines the number of seconds per day.
cSECONDS_PER_DAY :: Double
cSECONDS_PER_DAY = 86_400.0

cSECONDS_PER_DAY_I64 :: Int64
cSECONDS_PER_DAY_I64 = 86_400

-- | `SECONDS_PER_CENTURY` defines the number of seconds per century.
cSECONDS_PER_CENTURY :: Double
cSECONDS_PER_CENTURY = cSECONDS_PER_DAY * cDAYS_PER_CENTURY

-- | `SECONDS_PER_YEAR` corresponds to the number of seconds per julian year from [NAIF SPICE](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/jyear_c.html).
cSECONDS_PER_YEAR :: Double
cSECONDS_PER_YEAR = 31_557_600.0

cSECONDS_PER_YEAR_I64 :: Int64
cSECONDS_PER_YEAR_I64 = 31_557_600

-- | `SECONDS_PER_TROPICAL_YEAR` corresponds to the number of seconds per tropical year from [NAIF SPICE](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/tyear_c.html).
cSECONDS_PER_TROPICAL_YEAR :: Double
cSECONDS_PER_TROPICAL_YEAR = 31_556_925.974_7

-- | `SECONDS_PER_SIDERAL_YEAR` corresponds to the number of seconds per sideral year from [NIST](https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b9#TIME).
cSECONDS_PER_SIDERAL_YEAR :: Double
cSECONDS_PER_SIDERAL_YEAR = 31_558_150.0

-- | `SECONDS_GPS_TAI_OFFSET` is the number of seconds from the TAI epoch to the
-- GPS epoch (UTC midnight of January 6th 1980; cf. <https://gssc.esa.int/navipedia/index.php/Time_References_in_GNSS#GPS_Time_.28GPST.29>)
cSECONDS_GPS_TAI_OFFSET :: Double
cSECONDS_GPS_TAI_OFFSET =
    80.0 * cSECONDS_PER_YEAR + 4.0 * cSECONDS_PER_DAY + 19.0

cSECONDS_GPS_TAI_OFFSET_I64 :: Int64
cSECONDS_GPS_TAI_OFFSET_I64 =
    80 * cSECONDS_PER_YEAR_I64 + 4 * cSECONDS_PER_DAY_I64 + 19

-- | `DAYS_GPS_TAI_OFFSET` is the number of days from the TAI epoch to the GPS
-- epoch (UTC midnight of January 6th 1980; cf. <https://gssc.esa.int/navipedia/index.php/Time_References_in_GNSS#GPS_Time_.28GPST.29>)
cDAYS_GPS_TAI_OFFSET :: Double
cDAYS_GPS_TAI_OFFSET = cSECONDS_GPS_TAI_OFFSET / cSECONDS_PER_DAY
