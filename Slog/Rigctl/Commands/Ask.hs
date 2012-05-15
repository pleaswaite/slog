-- | This module provides the set of Ask commands.  An Ask command is one that is
-- passed to rigctl asking for a piece of data from the radio.  To use these commands,
-- provide one to 'Slog.Rigctl.Rigctl.ask'.  If there is a result, it will be provided
-- as a 'Slog.Rigctl.Commands.Tell.Command'.
module Slog.Rigctl.Commands.Ask(Command(..))
 where

import Slog.Rigctl.Commands.Class

-- | These are all the ask commands as supported by rigctl.
data Command = Frequency               -- ^ Ask for the frequency, in Hz.
             | Mode                    -- ^ Ask for the operating mode and passband, in Hz.
             | VFO                     -- ^ Ask for the current VFO.
             | RIT                     -- ^ Ask for the RIT, in Hz.
             | XIT                     -- ^ Ask for the XIT, in Hz.
             | PTT                     -- ^ Ask for the PTT status (receive or transmit)
             | DCD                     -- ^ Ask for the squelch status (closed or open).
             | RepeaterShift           -- ^ Ask for the repeater shift (+, -, or none).
             | RepeaterOffset          -- ^ Ask for the repeater offset, in Hz.
             | CTCSSTone               -- ^ Ask for the CTCSS tone, in tenths of Hz.
             | DCSCode                 -- ^ Ask for the DCS code.
             | CTCSSSql                -- ^ Ask for the CTCSS squelch tone, in tenths of Hz.
             | DCSSql                  -- ^ Ask for the DCS squelch code.
             | SplitFrequency          -- ^ Ask for the transmit frequency, in Hz.
             | SplitMode               -- ^ Ask for the split operating mode and passband, in Hz.
             | SplitVFO                -- ^ Ask for whether we are operating split and the transmit VFO.
             | TuningStep              -- ^ Ask for the tuning step, in Hz.
             | Function                -- ^ Ask for the value of some function.
             | Level                   -- ^ Ask for the level of some value.
             | Param                   -- ^ Ask for the value of some parameter.
               -- there is no get bank function
             | Memory                  -- ^ Ask for the memory channel number.
               -- there is no get mem/vfo op function
             | Channel                 -- ^ Not implemented in rigctld yet.
             | Transcieve              -- ^ Ask for the transcieve mode.
             | Antenna                 -- ^ Ask for the current antenna number.
               -- there is no get reset function
               -- there is no get morse function
             | PowerStatus             -- ^ Ask for the power status.
             | DTMF                    -- ^ Ask for the DTMF digits.
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
