module Slog.Formats.Cabrillo.ADIF(cabrilloToADIF)
 where

import Data.Maybe(mapMaybe)

import qualified Slog.Formats.ADIF.Types as A
import           Slog.Formats.Cabrillo.Contest.Class(CabrilloQSO(toADIF))
import qualified Slog.Formats.Cabrillo.Types as C

cabrilloToADIF :: C.CabrilloFile -> Maybe A.ADIFFile
cabrilloToADIF (C.CabrilloFile tags) = let
    fields = mapMaybe tagToADIF tags
 in
    if null fields then Nothing
    else Just A.ADIFFile { A.fileHeader=[], A.fileBody=fields }

tagToADIF :: C.Tag -> Maybe [A.Field]
tagToADIF t = case t of
    C.QSO q     -> toADIF q
    C.XQSO x    -> toADIF x
    _           -> Nothing
