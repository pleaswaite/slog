module Commands.Ask(Command(..))
 where

import Commands.Class

data Command = Frequency
             | Mode
             | VFO
             | RIT
             | XIT
             | PTT
             | DCD
             | RepeaterShift
             | RepeaterOffset
             | CTCSSTone
             | DCSCode
             | CTCSSSql
             | DCSSql
             | SplitFrequency
             | SplitMode
             | SplitVFO
             | TuningStep
             | Function
             | Level
             | Param
               -- there is no get bank function
             | Memory
               -- there is no get mem/vfo op function
             | Channel
             | Transcieve
             | Antenna
               -- there is no get reset function
               -- there is no get morse function
             | PowerStatus
             | DTMF
 deriving (Eq)

commandMap :: [(Command, String)]
commandMap = [(Frequency, "f"), (Mode, "m"), (VFO, "v"), (RIT, "j"), (XIT, "z"), (PTT, "t"),
              (DCD, "\\get_dcd"), (RepeaterShift, "r"), (RepeaterOffset, "o"), (CTCSSTone, "c"),
              (DCSCode, "d"), (CTCSSSql, "\\get_ctcss_sql"), (DCSSql, "\\get_dcs_sql"),
              (SplitFrequency, "i"), (SplitMode, "x"), (SplitVFO, "s"), (TuningStep, "n"),
              (Function, "u"), (Level, "l"), (Param, "p"), (Memory, "e"), (Channel, "h"),
              (Transcieve, "a"), (Antenna, "y"), (PowerStatus, "\\get_powerstatus"),
              (DTMF, "\\recv_dtmf")]

instance Serializable Command where
    ser cmd = lookup cmd commandMap
