module Slog.Rigctl.Commands.Class(Serializable(..))
 where

class Serializable c where
    ser :: c -> Maybe String