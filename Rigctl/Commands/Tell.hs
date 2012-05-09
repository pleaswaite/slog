module Commands.Tell(Command(..),
                     RigVFO(..),
                     Direction(..),
                     RigMode(..),
                     RigFunction(..),
                     RigLevel(..),
                     RigParam(..),
                     RigMVOp(..),
                     TranscieveMode(..))
 where

import Data.Bits
import Data.Maybe(fromJust, fromMaybe, isNothing)

import Commands.Class
import qualified Commands.Ask as A
import Utils(invert, stringToDouble, stringToInteger)

data Command = Frequency Integer
             | Mode RigMode Integer
             | VFO RigVFO
             | RIT Integer
             | XIT Integer
             | PTT Bool
             | DCD Bool       -- you can ask for the DCD value, but can't set it
             | RepeaterShift (Maybe Direction)
             | RepeaterOffset Integer
             | CTCSSTone Integer
             | DCSCode Integer
             | CTCSSSql Integer
             | DCSSql Integer
             | SplitFrequency Integer
             | SplitMode RigMode Integer
             | SplitVFO Bool RigVFO
             | TuningStep Integer
             | Function RigFunction Bool
             | Level RigLevel Double
             | Param RigParam Double
             | Bank Integer
             | Memory Integer
             | MVOp RigMVOp
             | Channel Integer
             | Transcieve TranscieveMode
             | Antenna Integer
             | Reset {resetNone :: Bool, resetSoftware :: Bool, resetVFO :: Bool,
                      resetMemoryClear :: Bool, resetMaster :: Bool}
             | Morse String
             | PowerStatus {powerOff :: Bool, powerOn :: Bool, powerStandby :: Bool}
             | DTMF String

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
    ser (Reset resetNone resetSoftware resetVFO resetMemoryClear resetMaster) =
        let i = 0
        in Just $ "\\reset " ++ show i
    ser (Morse s)          = Just $ "b " ++ s
    ser (PowerStatus powerOff powerOn powerStandby) =
        let i = 0
        in Just $ "\\set_powerstatus " ++ show i
    ser (DTMF s)           = Just $ "\\send_dtmf " ++ s
    ser _                  = Nothing

toTell :: A.Command -> [String] -> Maybe Command
toTell cmd s =
    case cmd of
        A.Frequency         -> toInt s 0 >>= Just . Frequency
        A.Mode              -> ifTwo s readMode
        A.VFO               -> Just $ VFO (read (s !! 0) :: RigVFO)
        A.RIT               -> toInt s 0 >>= Just . RIT
        A.XIT               -> toInt s 0 >>= Just . XIT
        A.PTT               -> toInt s 0 >>= \i -> Just $ PTT (i == 1)
        A.DCD               -> Nothing
        A.RepeaterShift     -> Just $ readDirection s
        A.RepeaterOffset    -> toInt s 0 >>= Just . RepeaterOffset
        A.CTCSSTone         -> toInt s 0 >>= Just . CTCSSTone
        A.DCSCode           -> toInt s 0 >>= Just . DCSCode
        A.CTCSSSql          -> toInt s 0 >>= Just . CTCSSSql
        A.DCSSql            -> toInt s 0 >>= Just . DCSSql
        A.SplitFrequency    -> toInt s 0 >>= Just . SplitFrequency
        A.SplitMode         -> ifTwo s readSplitMode
        A.SplitVFO          -> ifTwo s readSplitVFO
        A.TuningStep        -> toInt s 0 >>= Just . TuningStep
        A.Function          -> ifTwo s readFunc
        A.Level             -> ifTwo s readLevel
        A.Param             -> ifTwo s readParam
        A.Memory            -> toInt s 0 >>= Just . Memory
        A.Channel           -> toInt s 0 >>= Just . Channel
        A.Transcieve        -> Just $ Transcieve (read (s !! 0) :: TranscieveMode)
        A.Antenna           -> toInt s 0 >>= Just . Antenna
        A.PowerStatus       -> toInt s 0 >>= \i -> Just PowerStatus {powerOff     = i == 0,
                                                                     powerOn      = testBit i 1,
                                                                     powerStandby = testBit i 2}
        A.DTMF              -> Just $ DTMF (s !! 0)
 where
    first lst = fst (lst !! 0)

    ifNotEmpty l v | length l == 0 = Nothing
                   | otherwise     = Just v

    ifTwo l f | length l == 2 = f l
              | otherwise     = Nothing

    toInt s n = stringToInteger (s !! n)

    readDirection [l1] = let v1 = reads l1 :: [(Direction, String)]
                         in RepeaterShift $ ifNotEmpty v1 (first v1)

    readMode [l1, l2] = let v1 = reads l1 :: [(RigMode, String)]
                        in stringToInteger l2 >>= \i -> ifNotEmpty v1 (Mode (first v1) i)

    readSplitMode [l1, l2] = let v1 = reads l1 :: [(RigMode, String)]
                             in stringToInteger l2 >>= \i -> ifNotEmpty v1 (SplitMode (first v1) i)

    readSplitVFO [l1, l2] = let v2 = reads l2 :: [(RigVFO, String)]
                            in stringToInteger l1 >>= \i -> ifNotEmpty v2 (SplitVFO (i == 1) (first v2))

    readFunc [l1, l2] = let v1 = reads l1 :: [(RigFunction, String)]
                        in stringToInteger l2 >>= \i -> ifNotEmpty v1 (Function (first v1) (i == 1))

    readLevel [l1, l2] = let v1 = reads l1 :: [(RigLevel, String)]
                         in stringToDouble l2 >>= \d -> ifNotEmpty v1 (Level (first v1) d)

    readParam [l1, l2] = let v1 = reads l1 :: [(RigParam, String)]
                         in stringToDouble l2 >>= \d -> ifNotEmpty v1 (Param (first v1) d)

--

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

data Direction = Plus | Minus
    deriving (Eq)

instance Show Direction where
    show Plus = "+"
    show Minus = "-"

instance Read Direction where
    readsPrec _ "+" = [(Plus, "")]
    readsPrec _ "-" = [(Minus, "")]

--

data RigMode = USB | LSB | CW | CWR | RTTY | RTTYR | AM | FM | WFM | AMS | PKTLSB |
               PKTUSB | PKTFM | ECSSUSB | ECSSLSB | FAX | SAM | SAL | SAH | DSB
    deriving (Eq, Show, Read)

--

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

data RigParam = ANN | APO | BACKLIGHT | BEEP | TIME | BAT | KEYLIGHT
    deriving (Eq, Show, Read)

--

data RigMVOp = CPY | XCHG | FROM_VFO | TO_VFO | MCL | UP | DOWN | BAND_UP | BAND_DOWN |
               LEFT | RIGHT | TUNE | TOGGLE
    deriving (Eq, Show, Read)

--

data TranscieveMode = OFF | RIG | POLL
    deriving (Eq, Show, Read)
