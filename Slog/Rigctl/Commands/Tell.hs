-- | This module provides the set of Tell commands.  A Tell command is used for one of
-- two things:  First, it is the result of the radio previously being asked for a piece
-- of data - that is, the radio is telling you something.  Second, it is used to have
-- the radio set some value or perform some function - that is, you are telling the radio
-- to do something.
--
-- To use these commands, provide one to 'Slog.Rigctl.Rigctl.tell'.  The result is an
-- error code, if any.  Note that not all radios support all commands.
module Slog.Rigctl.Commands.Tell(Command(..),
                                 RigVFO(..),
                                 Direction(..),
                                 RigMode(..),
                                 RigFunction(..),
                                 RigLevel(..),
                                 RigParam(..),
                                 RigMVOp(..),
                                 TranscieveMode(..),
                                 toTell)
 where

import Data.Bits
import Data.Maybe(fromMaybe)

import Slog.Rigctl.Commands.Class
import qualified Slog.Rigctl.Commands.Ask as A
import Slog.Utils(invert, stringToDouble, stringToInteger)

-- | These are all the tell commands as supported by rigctl.
data Command = Frequency Integer                -- ^ Set the frequency, in Hz.
             | Mode RigMode Integer             -- ^ Set the operating mode and passband, in Hz.
             | VFO RigVFO                       -- ^ Set the current VFO.
             | RIT Integer                      -- ^ Set the RIT, in Hz.
             | XIT Integer                      -- ^ Set the XIT, in Hz.
             | PTT Bool                         -- ^ Set the PTT status (transmit = True)
             | DCD Bool                         -- ^ Return the result of asking for DCD.  This value cannot be set.
             | RepeaterShift (Maybe Direction)  -- ^ Set the repeater shift direction, if any.
             | RepeaterOffset Integer           -- ^ Set the repeater offset, in Hz.
             | CTCSSTone Integer                -- ^ Set the CTCSS tone, in tenths of Hz.
             | DCSCode Integer                  -- ^ Set the DCS code.
             | CTCSSSql Integer                 -- ^ Set the CTCSS squelch tone, in tenths of Hz.
             | DCSSql Integer                   -- ^ Set the DCS squelch code.
             | SplitFrequency Integer           -- ^ Set the transmit frequency, in Hz.
             | SplitMode RigMode Integer        -- ^ Set the split operating mode and passband, in Hz.
             | SplitVFO Bool RigVFO             -- ^ Set whether we are operating split and the transmit VFO.
             | TuningStep Integer               -- ^ Set the tuning step, in Hz.
             | Function RigFunction Bool        -- ^ Set some radio function to be on or off.
             | Level RigLevel Double            -- ^ Set the level of some value.
             | Param RigParam Double            -- ^ Set the value of some parameter.
             | Bank Integer                     -- ^ Set the current memory bank.
             | Memory Integer                   -- ^ Set the current memory channel.
             | MVOp RigMVOp                     -- ^ Perform some memory/VFO operation.
             | Channel Integer                  -- ^ Not implemented in rigctld yet.
             | Transcieve TranscieveMode        -- ^ Set the transcieve mode.
             | Antenna Integer                  -- ^ Set the current antenna.
             | Reset {resetNone :: Bool, resetSoftware :: Bool, resetVFO :: Bool,
                      resetMemoryClear :: Bool, resetMaster :: Bool}
                                                -- ^ Send a reset of various parts of the radio.
             | Morse String                     -- ^ Send a string of morse code.
             | PowerStatus {powerOff :: Bool, powerOn :: Bool, powerStandby :: Bool}
                                                -- ^ Set the radio's power status.
             | DTMF String                      -- ^ Send DTMF digits.

instance Serializable Command where
    ser (Frequency i)      = Just $ "F " ++ show i
    ser (Mode m i)         = Just $ "M " ++ show m ++ " " ++ show i
    ser (VFO v)            = Just $ "V " ++ show v
    ser (RIT i)            = Just $ "J " ++ show i
    ser (XIT i)            = Just $ "Z " ++ show i
    ser (PTT b)            = Just $ "T " ++ if b then "1" else "0"
    ser (RepeaterShift d)  = Just $ "R " ++ (maybe "" show d)
    ser (RepeaterOffset i) = Just $ "O " ++ show i
    ser (CTCSSTone i)      = Just $ "C " ++ show i
    ser (DCSCode i)        = Just $ "D " ++ show i
    ser (CTCSSSql i)       = Just $ "\\set_ctcss_sql " ++ show i
    ser (DCSSql i)         = Just $ "\\set_dcs_sql " ++ show i
    ser (SplitFrequency i) = Just $ "I " ++ show i
    ser (SplitMode m i)    = Just $ "X " ++ show m ++ " " ++ show i
    ser (SplitVFO b v)     = Just $ "S " ++ if b then "1 " else "0 " ++ show v
    ser (TuningStep i)     = Just $ "N " ++ show i
    ser (Function f b)     = Just $ "U " ++ show f ++ if b then "1" else "0"
    ser (Level l f)        = Just $ "L " ++ show l ++ " " ++ show f
    ser (Param p i)        = Just $ "P " ++ show p ++ " " ++ show i
    ser (Bank i)           = Just $ "B " ++ show i
    ser (Memory i)         = Just $ "E " ++ show i
    ser (MVOp s)           = Just $ "G " ++ show s
    ser (Channel i)        = Just $ "H " ++ show i
    ser (Transcieve s)     = Just $ "A " ++ show s
    ser (Antenna i)        = Just $ "Y " ++ show i
    ser (Reset _ _ _ _ _)  = Just $ "\\reset 0"
    ser (Morse s)          = Just $ "b " ++ s
    ser (PowerStatus _ _ _) = Just $ "\\set_powerstatus 0"
    ser (DTMF s)           = Just $ "\\send_dtmf " ++ s
    ser _                  = Nothing

-- | Given the ask 'Slog.Rigctl.Commands.Ask.Command' and the response from running that
-- ask command, convert it into a tell 'Slog.Rigctl.Commands.Tell.Command'.  Because not all
-- ask commands have an equivalent tell command, the result must be wrapped in a 'Maybe'.
-- This also presumes the response has already been checked for an error code.
toTell :: A.Command -> [String] -> Maybe Command
toTell cmd str =
    case cmd of
        A.Frequency         -> toInt str 0 >>= Just . Frequency
        A.Mode              -> ifTwo str readMode
        A.VFO               -> Just $ VFO (read (str !! 0) :: RigVFO)
        A.RIT               -> toInt str 0 >>= Just . RIT
        A.XIT               -> toInt str 0 >>= Just . XIT
        A.PTT               -> toInt str 0 >>= \i -> Just $ PTT (i == 1)
        A.DCD               -> Nothing
        A.RepeaterShift     -> readDirection str
        A.RepeaterOffset    -> toInt str 0 >>= Just . RepeaterOffset
        A.CTCSSTone         -> toInt str 0 >>= Just . CTCSSTone
        A.DCSCode           -> toInt str 0 >>= Just . DCSCode
        A.CTCSSSql          -> toInt str 0 >>= Just . CTCSSSql
        A.DCSSql            -> toInt str 0 >>= Just . DCSSql
        A.SplitFrequency    -> toInt str 0 >>= Just . SplitFrequency
        A.SplitMode         -> ifTwo str readSplitMode
        A.SplitVFO          -> ifTwo str readSplitVFO
        A.TuningStep        -> toInt str 0 >>= Just . TuningStep
        A.Function          -> ifTwo str readFunc
        A.Level             -> ifTwo str readLevel
        A.Param             -> ifTwo str readParam
        A.Memory            -> toInt str 0 >>= Just . Memory
        A.Channel           -> toInt str 0 >>= Just . Channel
        A.Transcieve        -> Just $ Transcieve (read (str !! 0) :: TranscieveMode)
        A.Antenna           -> toInt str 0 >>= Just . Antenna
        A.PowerStatus       -> toInt str 0 >>= \i -> Just PowerStatus {powerOff     = i == 0,
                                                                       powerOn      = testBit i 1,
                                                                       powerStandby = testBit i 2}
        A.DTMF              -> Just $ DTMF (str !! 0)
 where
    first lst = fst (lst !! 0)

    ifNotEmpty l v | length l == 0 = Nothing
                   | otherwise     = Just v

    ifTwo l f | length l == 2 = f l
              | otherwise     = Nothing

    toInt s n = stringToInteger (s !! n)

    readDirection [l1] = let v1 = reads l1 :: [(Direction, String)]
                         in Just $ RepeaterShift $ ifNotEmpty v1 (first v1)
    readDirection _    = Nothing

    readMode [l1, l2] = let v1 = reads l1 :: [(RigMode, String)]
                        in stringToInteger l2 >>= \i -> ifNotEmpty v1 (Mode (first v1) i)
    readMode _        = Nothing

    readSplitMode [l1, l2] = let v1 = reads l1 :: [(RigMode, String)]
                             in stringToInteger l2 >>= \i -> ifNotEmpty v1 (SplitMode (first v1) i)
    readSplitMode _        = Nothing

    readSplitVFO [l1, l2] = let v2 = reads l2 :: [(RigVFO, String)]
                            in stringToInteger l1 >>= \i -> ifNotEmpty v2 (SplitVFO (i == 1) (first v2))
    readSplitVFO _        = Nothing

    readFunc [l1, l2] = let v1 = reads l1 :: [(RigFunction, String)]
                        in stringToInteger l2 >>= \i -> ifNotEmpty v1 (Function (first v1) (i == 1))
    readFunc _        = Nothing

    readLevel [l1, l2] = let v1 = reads l1 :: [(RigLevel, String)]
                         in stringToDouble l2 >>= \d -> ifNotEmpty v1 (Level (first v1) d)
    readLevel _        = Nothing

    readParam [l1, l2] = let v1 = reads l1 :: [(RigParam, String)]
                         in stringToDouble l2 >>= \d -> ifNotEmpty v1 (Param (first v1) d)
    readParam _        = Nothing

-- | This data type is used to represent VFOs in various places.  Not all radios will
-- support all these values.
data RigVFO = V_VFOA | V_VFOB | V_VFOC | V_Current | V_VFO | V_MEM | V_Main | V_Sub | V_TX | V_RX
    deriving (Eq)

vfoMap :: [(RigVFO, String)]
vfoMap = [(V_VFOA, "VFOA"), (V_VFOB, "VFOB"), (V_VFOC, "VFOC"), (V_Current, "currVFO"), (V_VFO, "VFO"),
          (V_MEM, "MEM"), (V_Main, "MAIN"), (V_Sub, "SUB"), (V_TX, "TX"), (V_RX, "RX")]

vfoMap' :: [(String, RigVFO)]
vfoMap' = invert vfoMap

instance Show RigVFO where
    show vfo = fromMaybe "" (lookup vfo vfoMap)

instance Read RigVFO where
    readsPrec _ vfo = maybe [] (\s -> [(s, "")]) (lookup vfo vfoMap')

--

-- | This data type is used for expressing repeater shift directions, if any.
data Direction = Plus | Minus
    deriving (Eq)

instance Show Direction where
    show Plus = "+"
    show Minus = "-"

instance Read Direction where
    readsPrec _ "+" = [(Plus, "")]
    readsPrec _ "-" = [(Minus, "")]

--

-- | This data type is used for expressing the operating mode.  Not all radios will
-- support all these values.  Also note that these are a subset of those expressable
-- via 'Slog.Formats.ADIF.Types.Mode'.
data RigMode = USB | LSB | CW | CWR | RTTY | RTTYR | AM | FM | WFM | AMS | PKTLSB |
               PKTUSB | PKTFM | ECSSUSB | ECSSLSB | FAX | SAM | SAL | SAH | DSB
    deriving (Eq, Show, Read)

--

-- | This data type expresses a variety of functions that can be set on a radio.  Not
-- all radios will support all functions.
data RigFunction = F_FAGC | F_NB | F_COMP | F_VOX | F_TONE | F_TSQL | F_SBKIN | F_FBKIN | F_ANF | F_NR |
                   F_AIP | F_APF | F_MON | F_MN | F_RF | F_ARO | F_LOCK | F_MUTE | F_VSC | F_REV | F_SQL |
                   F_ABM | F_BC | F_MBC | F_AFC | F_SATMODE | F_SCOPE | F_RESUME | F_TBURST | F_TUNER
    deriving (Eq)

functionMap :: [(RigFunction, String)]
functionMap = []

functionMap' :: [(String, RigFunction)]
functionMap' = invert functionMap

instance Show RigFunction where
    show func = fromMaybe "" (lookup func functionMap)

instance Read RigFunction where
    readsPrec _ func = maybe [] (\s -> [(s, "")]) (lookup func functionMap')

--

-- | This data type expresses a variety of levels that can be set on a radio.  Not all
-- radios will support all levels.
data RigLevel = L_PREAMP | L_ATT | L_VOX | L_AF | L_RF | L_SQL | L_IF | L_APF | L_NR | L_PBT_IN | L_PBT_OUT |
                L_CWPITCH | L_RFPOWER | L_MICGAIN | L_KEYSPD | L_NOTCHF | L_COMP | L_AGC | L_BKINDL | L_BAL |
                L_METER | L_VOXGAIN | L_ANTIVOX | L_SLOPE_LOW | L_SLOPE_HIGH | L_RAWSTR | L_SQLSTAT |
                L_SWR | L_ALC | L_STRENGTH
    deriving (Eq)

levelMap :: [(RigLevel, String)]
levelMap = []

levelMap' :: [(String, RigLevel)]
levelMap' = invert levelMap

instance Show RigLevel where
    show lvl = fromMaybe "" (lookup lvl levelMap)

instance Read RigLevel where
    readsPrec _ lvl = maybe [] (\s -> [(s, "")]) (lookup lvl levelMap')

--

-- | This data type expresses a variety of parameters that can be set on a radio.  Not
-- all radios will support all parameters.
data RigParam = ANN | APO | BACKLIGHT | BEEP | TIME | BAT | KEYLIGHT
    deriving (Eq, Show, Read)

--

-- | This data type expresses a variety of memory/VFO operations that can be performed
-- by a radio.  Not all radios will support all operations.
data RigMVOp = CPY | XCHG | FROM_VFO | TO_VFO | MCL | UP | DOWN | BAND_UP | BAND_DOWN |
               LEFT | RIGHT | TUNE | TOGGLE
    deriving (Eq, Show, Read)

--

-- | This data type expresses the transciever modes.
data TranscieveMode = OFF | RIG | POLL
    deriving (Eq, Show, Read)
