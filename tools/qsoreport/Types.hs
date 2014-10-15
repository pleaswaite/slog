module Types where

import Slog.QSO

type EntryTy = Bool

data BandRow = BandRow {
    row160M :: EntryTy,
    row80M :: EntryTy,
    row60M :: EntryTy,
    row40M :: EntryTy,
    row30M :: EntryTy,
    row20M :: EntryTy,
    row17M :: EntryTy,
    row15M :: EntryTy,
    row12M :: EntryTy,
    row10M :: EntryTy,
    row6M :: EntryTy,
    row2M :: EntryTy,
    row1Point25M :: EntryTy,
    row70CM :: EntryTy
 } deriving (Show)

emptyBandRow :: BandRow
emptyBandRow = BandRow { row160M=False, row80M=False, row60M=False, row40M=False, row30M=False,
                         row20M=False, row17M=False, row15M=False, row12M=False, row10M=False,
                         row6M=False, row2M=False, row1Point25M=False, row70CM=False }
