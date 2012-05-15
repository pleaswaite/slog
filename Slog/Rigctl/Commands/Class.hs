-- | This module provides a type class for serializing Commands.
module Slog.Rigctl.Commands.Class(Serializable(..))
 where

-- | The Serializable class defined a single 'ser' method that attempts to convert
-- an instance into a 'String'.
class Serializable c where
    ser :: c -> Maybe String
