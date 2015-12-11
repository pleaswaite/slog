module Slog.Formats.Utils(cabrilloBandToADIF)
 where

import qualified Slog.Formats.ADIF.Types as A
import qualified Slog.Formats.Cabrillo.Types as C

cabrilloBandToADIF :: C.Band -> Maybe A.Field
cabrilloBandToADIF m = case m of
    C.Band160M      -> Just $ A.Band A.Band160M
    C.Band80M       -> Just $ A.Band A.Band80M
    C.Band40M       -> Just $ A.Band A.Band40M
    C.Band20M       -> Just $ A.Band A.Band20M
    C.Band15M       -> Just $ A.Band A.Band15M
    C.Band10M       -> Just $ A.Band A.Band10M
    C.Band6M        -> Just $ A.Band A.Band6M
    C.Band2M        -> Just $ A.Band A.Band2M
    C.Band222       -> Just $ A.Band A.Band1Point25M
    C.Band432       -> Just $ A.Band A.Band70CM
    C.Band902       -> Just $ A.Band A.Band33CM
    C.Band1Point2G  -> Just $ A.Band A.Band23CM
    C.Band2Point3G  -> Just $ A.Band A.Band13CM
    C.Band3Point4G  -> Just $ A.Band A.Band9CM
    C.Band5Point7G  -> Just $ A.Band A.Band6CM
    C.Band10G       -> Just $ A.Band A.Band3CM
    C.Band24G       -> Just $ A.Band A.Band1Point25CM
    C.Band47G       -> Just $ A.Band A.Band6MM
    C.Band75G       -> Just $ A.Band A.Band4MM
    C.Band119G      -> Just $ A.Band A.Band2Point5MM
    C.Band142G      -> Just $ A.Band A.Band2MM
    C.Band241G      -> Just $ A.Band A.Band1MM
    C.BandFreq f    -> Just $ A.Freq (fromIntegral f)
    _               -> Nothing
