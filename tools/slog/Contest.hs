{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | The Contest module provides data types and functions used to generate the
-- outgoing exchange for a contest.  A user simply calls one of the mk*Contest
-- functions to generate a new type with the initial value.  Then each time a new
-- value is needed, successive calls to 'contestNext' will generate a new type
-- with the next value.
--
-- This module supports both well-known contests (like the ARRL Sweepstakes,
-- CQ WW DX, and ARRL VHF/UHF contests), as well as generic serial number-based
-- and grid-based contests.  This makes up a majority of the contests I care about.
module Contest where

import Text.Printf(printf)

-- | A 'Contest' data type is the central data type for this module.  It packages up
-- an initial value plus ways to get the next value and a string representation
-- suitable for logging (thus why we don't simply derive 'Show' here).  Callers
-- should probably not manipulate values of this type directly, but instead use the
-- other functions provided in this module.
data Contest = forall a. MkContest
    a                   -- ^ new - the initial value
    (a -> a)            -- ^ next - given a value of this type, return the next exchange
    (a -> String)       -- ^ str - given a value of this type, return it as a string

-- | The ARRL Sweepstakes has a very complicated exchange, so it needs its own data
-- type.  All values are required, even the callsign (which is our callsign, and not
-- that of the remote station).
data Sweeps = Sweeps { swSerial :: Integer,    -- ^ the serial number of this exchange
                       swPrec :: Char,         -- ^ a precedence letter (see ARRL docs)
                       swCall :: String,       -- ^ our call sign
                       swCheck :: Integer,     -- ^ the last two digits of year of license
                       swSection :: String     -- ^ ARRL section (see ARRL docs)
 }

-- | Return a 'Contest' value for when there is no contest.  This is useful so calling
-- code does not have to conditionalize being in contest mode - it can just use the result
-- of this function.  The string representation will always be "".
--
-- This function takes a string as input, but does nothing with it.
mkNoneContest :: String -> Contest
mkNoneContest new = MkContest new id (const "")

-- | Return a 'Contest' value for grid-based contests.  This applies to both the various
-- ARRL VHF/UHF contests throughout the year, as well as any other contest that requires
-- a grid for exchange.  There are several smaller ones that fit.
--
-- The input string is the grid, which will always be the next value and the string
-- representation.
mkGridContest :: String -> Contest
mkGridContest new = MkContest new id id

-- | Return a 'Contest' value for serial number-based contests.  There are an awful lot of
-- these throughout the year, so this function is probably the most useful one.
--
-- The input value is the initial serial number (which could be something other than 1 if
-- the user quits partway through the contests and then picks it back up later on), and
-- the next value will just be the next number.  The string representation is padded out
-- to three places as that's how most people expect it.
mkSerialContest :: Integer -> Contest
mkSerialContest new = MkContest new (+1) (printf "%03d")

-- | Return a 'Contest' value for the ARRL Sweepstakes.  This is an awfully complicated
-- exchange.
--
-- The initial value is a 'Sweeps' record.  The next value will increment the serial number,
-- and the string representation will be formatted as expected.
mkSweepsContest :: Sweeps -> Contest
mkSweepsContest new = MkContest new (\s -> s { swSerial=swSerial s + 1 })
                                    (\s -> printf "%03d %c %s %02d %s" (swSerial s) (swPrec s) (swCall s)
                                                                       (swCheck s) (swSection s))

-- | Return a 'Contest' value for the CQ WW DX contest.  This could also apply to any other
-- contest that expects the CQ Zone as the exchange, which is likely not very many.  This is
-- the input value, which will always be the next value and the string representation.
mkZoneContest :: Integer -> Contest
mkZoneContest new = MkContest new id (printf "%02d")

-- | Given a 'Contest' value, generate and return another 'Contest' value with the next
-- exchange packed into it.  It is much preferred that users of this module generate
-- successive exchanges via this function instead of unpacking the 'Contest' value and
-- figuring it out manually.
contestNext :: Contest -> Contest
contestNext (MkContest new next str) = MkContest (next new) next str

-- | Given a 'Contest' value, generate the string representation of it.  This is suitable
-- for displaying in a UI or logging into the database.
contestStr :: Contest -> String
contestStr (MkContest new _ str) = str new
