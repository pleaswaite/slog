{-# OPTIONS_GHC -Wall #-}

-- | A module for backing up the database.
module Slog.Backups(backup,
                    expire)
 where

import Control.Applicative((<$>))
import Control.Conditional(whenM)
import Control.Monad(when)
import Data.List(isSuffixOf, sort)
import Data.Time.Clock(UTCTime(utctDay, utctDayTime), getCurrentTime)
import System.Directory(copyFile, doesFileExist, getDirectoryContents, removeFile)
import System.FilePath.Posix((</>), splitFileName)
import Text.Printf(printf)

import Slog.Utils(undashifyDate)

-- | Given a database location, create a backup of it in the same directory.
backup :: FilePath -> IO ()
backup db = whenM (doesFileExist db) $ do
    let (dir, file) = splitFileName db
    today <- getCurrentTime
    let file' = printf "%s-%d-%s" (undashifyDate $ show $ utctDay today)
                                  ((floor $ toRational $ utctDayTime today) :: Integer)
                                  file
    copyFile db (dir </> file')

-- | Enforce a limit on the number of backups of the database by deleting the
-- oldest copies until the limit is reached.
expire :: FilePath -> Int -> IO ()
expire db limit = whenM (doesFileExist db) $ do
    let (dir, file) = splitFileName db
    contents <- sort . filter (("-" ++ file) `isSuffixOf`) <$> getDirectoryContents dir

    when (length contents > limit) $ do
        let toRemove = take (length contents - limit) contents
        mapM_ (\f -> removeFile $ dir </> f) toRemove
