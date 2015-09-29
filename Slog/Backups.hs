{-# OPTIONS_GHC -Wall #-}

-- | A module for backing up the database.
module Slog.Backups(backup,
                    expire)
 where

import Control.Applicative((<$>))
import Control.Monad(when)
import Data.List(isSuffixOf, sort)
import Data.Time.Clock(UTCTime(utctDay), getCurrentTime)
import System.Directory(copyFile, getDirectoryContents, removeFile)
import System.FilePath.Posix((</>), splitFileName)

import Slog.Utils(undashifyDate)

-- | Given a database location, create a backup of it in the same directory.
backup :: FilePath -> IO ()
backup db = do
    let (dir, file) = splitFileName db
    today <- getCurrentTime
    let file' = undashifyDate (show $ utctDay today) ++ "-" ++ file
    copyFile db (dir </> file')

-- | Enforce a limit on the number of backups of the database by deleting the
-- oldest copies until the limit is reached.
expire :: FilePath -> Int -> IO ()
expire db limit = do
    let (dir, file) = splitFileName db
    contents <- sort . filter (("-" ++ file) `isSuffixOf`) <$> getDirectoryContents dir

    when (length contents > limit) $ do
        let toRemove = take (length contents - limit) contents
        mapM_ removeFile toRemove
