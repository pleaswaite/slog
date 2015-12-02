{-# LANGUAGE ExistentialQuantification #-}

module Slog.Formats.Cabrillo.Types(Assistance(..),
                                   Band(..),
                                   CabrilloFile(..),
                                   Date,
                                   Mode(..),
                                   Operator(..),
                                   Overlay(..),
                                   Power(..),
                                   Station(..),
                                   Tag(..),
                                   Time,
                                   TimeOperated(..),
                                   Transmitter(..))
 where

import Data.Maybe(fromMaybe)

import Slog.Formats.Cabrillo.Contest.Class(CabrilloQSO)
import Slog.Utils(invert, uppercase)

readsError :: a -> [(a, String)]
readsError x = [(x, "")]

--
-- BASIC FIELD DATA TYPES
--

-- | Date is represented by a 'String', and should be in YYYYMMDD format.
type Date = String

-- | Time is represented by a 'String', and should be in HHMM format.
type Time = String

--
-- TOP-LEVEL DATA TYPE
--

-- | This record is the top-level representation of a Cabrillo file.  It consists of a
-- list of 'Tag's, some of which represent QSOs that happened in the contest and some of
-- which describe other data about the operator and the conditions of the contest.
--
-- Converting a 'String' into a 'CabrilloFile' is accomplished via the 'Parser' module,
-- while converting a 'CabrilloFile' into a 'String' is accomplished via the 'Writer' module.
data CabrilloFile = CabrilloFile [Tag]

--
-- TAGS
--
data Tag = StartOfLog             String
         | EndOfLog
         | Callsign               String
         | Contest                String
         | CategoryAssisted       Assistance
         | CategoryBand           Band
         | CategoryMode           Mode
         | CategoryOperator       Operator
         | CategoryPower          Power
         | CategoryStation        Station
         | CategoryTime           TimeOperated
         | CategoryTransmitter    Transmitter
         | CategoryOverlay        Overlay
         | Certificate            Bool
         | ClaimedScore           Integer
         | Club                   String
         | CreatedBy              String
         | Email                  String
         | Location               String
         | Name                   String
         | Address                [String]
         | AddressCity            String
         | AddressStateProvince   String
         | AddressPostalCode      String
         | AddressCountry         String
         | Operators              [String]
         | OffTime                Time Time
         | Soapbox                [String]
         | XLine                  String String
         | Debug                  Integer
         | forall a. (CabrilloQSO a) => QSO a
         | forall a. (CabrilloQSO a) => XQSO a

-- | Was the operated assisted in some way?
data Assistance = Assisted | NonAssisted
 deriving (Eq)

assistanceMap :: [(Assistance, String)]
assistanceMap = [(Assisted, "ASSISTED"), (NonAssisted, "NON-ASSISTED")]

assistanceMap' :: [(String, Assistance)]
assistanceMap' = invert assistanceMap

instance Show Assistance where
    show t = fromMaybe "" (lookup t assistanceMap)

instance Read Assistance where
    readsPrec _ t = maybe [] readsError (lookup (uppercase t) assistanceMap')

data Band = BandAll | Band160M | Band80M | Band40M | Band20M | Band15M | Band10M
          | Band6M  | Band2M | Band222 | Band432 | Band902 | Band1Point2G | Band2Point3G
          | Band3Point4G | Band5Point7G | Band10G | Band24G | Band47G | Band75G
          | Band119G | Band142G | Band241G | BandLight | Band3Band | BandFMOnly
          | BandFreq Integer
 deriving (Eq, Ord)

bandMap :: [(Band, String)]
bandMap = [(BandAll, "ALL"), (Band160M, "160M"), (Band80M, "80M"), (Band40M, "40M"), (Band20M, "20M"),
           (Band15M, "15M"), (Band10M, "10M"), (Band6M, "6M"), (Band2M, "2M"), (Band222, "222"),
           (Band432, "432"), (Band902, "902"), (Band1Point2G, "1.2G"), (Band2Point3G, "2.3G"),
           (Band3Point4G, "3.4G"), (Band5Point7G, "5.7G"), (Band10G, "10G"), (Band24G, "24G"),
           (Band47G, "47G"), (Band75G, "75G"), (Band119G, "119G"), (Band142G, "142G"), (Band241G, "241G"),
           (BandLight, "Light"), (Band3Band, "VHF-3-BAND"), (BandFMOnly, "VHF-FM-ONLY")]

bandMap' :: [(String, Band)]
bandMap' = invert bandMap

instance Show Band where
    show (BandFreq i) = show i
    show t            = fromMaybe "" (lookup t bandMap)

instance Read Band where
    readsPrec _ t = case lookup (uppercase t) bandMap' of
                        Just b  -> [(b, "")]
                        Nothing -> [(BandFreq (read t :: Integer), "")]

data Mode = FM | CW | PH | RY
 deriving (Eq, Read, Show)

data Operator = SingleOp | MultiOp | Checklog
 deriving (Eq)

operatorMap :: [(Operator, String)]
operatorMap = [(SingleOp, "SINGLE-OP"), (MultiOp, "MULTI-OP"), (Checklog, "CHECKLOG")]

operatorMap' :: [(String, Operator)]
operatorMap' = invert operatorMap

instance Show Operator where
    show t = fromMaybe "" (lookup t operatorMap)

instance Read Operator where
    readsPrec _ t = maybe [] readsError (lookup (uppercase t) operatorMap')

data Power = PowerHigh | PowerLow | PowerQRP
 deriving (Eq)

powerMap :: [(Power, String)]
powerMap = [(PowerHigh, "HIGH"), (PowerLow, "LOW"), (PowerQRP, "QRP")]

powerMap' :: [(String, Power)]
powerMap' = invert powerMap

instance Show Power where
    show t = fromMaybe "" (lookup t powerMap)

instance Read Power where
    readsPrec _ t = maybe [] readsError (lookup (uppercase t) powerMap')

data Station = Fixed | Mobile | Portable | Rover | RoverLimited | RoverUnlimited
             | Expedition | HQ | School
 deriving (Eq)

stationMap :: [(Station, String)]
stationMap = [(Fixed, "FIXED"), (Mobile, "MOBILE"), (Portable, "PORTABLE"), (Rover, "ROVER"),
              (RoverLimited, "ROVER-LIMITED"), (RoverUnlimited, "ROVER-UNLIMITED"),
              (Expedition, "EXPEDITION"), (HQ, "HQ"), (School, "SCHOOL")]

stationMap' :: [(String, Station)]
stationMap' = invert stationMap

instance Show Station where
    show t = fromMaybe "" (lookup t stationMap)

instance Read Station where
    readsPrec _ t = maybe [] readsError (lookup (uppercase t) stationMap')

data TimeOperated = SixHours | TwelveHours | TwentyFourHours
 deriving (Eq)

timeOperatedMap :: [(TimeOperated, String)]
timeOperatedMap = [(SixHours, "6-HOURS"), (TwelveHours, "12-HOURS"), (TwentyFourHours, "24-HOURS")]

timeOperatedMap' :: [(String, TimeOperated)]
timeOperatedMap' = invert timeOperatedMap

instance Show TimeOperated where
    show t = fromMaybe "" (lookup t timeOperatedMap)

instance Read TimeOperated where
    readsPrec _ t = maybe [] readsError (lookup (uppercase t) timeOperatedMap')

data Transmitter = OneTransmitter | TwoTransmitters | LimitedTransmitters
                 | UnlimitedTransmitters | SWL
 deriving (Eq)

transmitterMap :: [(Transmitter, String)]
transmitterMap = [(OneTransmitter, "ONE"), (TwoTransmitters, "TWO"), (LimitedTransmitters, "LIMITED"),
                  (UnlimitedTransmitters, "UNLIMITED"), (SWL, "SWL")]

transmitterMap' :: [(String, Transmitter)]
transmitterMap' = invert transmitterMap

instance Show Transmitter where
    show t = fromMaybe "" (lookup t transmitterMap)

instance Read Transmitter where
    readsPrec _ t = maybe [] readsError (lookup (uppercase t) transmitterMap')

data Overlay = Classic | Rookie | TBWires | NoviceTech | Over50
 deriving (Eq)

overlayMap :: [(Overlay, String)]
overlayMap = [(Classic, "CLASSIC"), (Rookie, "ROOKIE"), (TBWires, "TB-WIRES"), (NoviceTech, "NOVICE-TECH"), (Over50, "OVER-50")]

overlayMap' :: [(String, Overlay)]
overlayMap' = invert overlayMap

instance Show Overlay where
    show o = fromMaybe "" (lookup o overlayMap)

instance Read Overlay where
    readsPrec _ o = maybe [] readsError (lookup (uppercase o) overlayMap')
