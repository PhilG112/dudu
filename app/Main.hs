{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AppRWST
import AppTypes (AppConfig)
import qualified Data.Text.IO as TIO
import DirTree
import DiskUsage
import System.Posix.Types
import TextShow
import Utils

main :: IO ()
main = pure ()

work :: AppConfig -> IO ()
work config = do
    (_, dirs) <- runMyApp dirTree config ()
    (_, counters) <- runMyApp fileCount config ()
    (_, usages) <- runMyApp diskUsage config (0 :: FileOffset)
    let report =
            toText $
                buildEntries "Directory tree:" treeEntryBuilder dirs
                    <> buildEntries "File counter:" tabEntryBuilder counters
                    <> buildEntries "File space usage:" tabEntryBuilder usages
    TIO.putStr report

buildEntries :: Builder -> (e -> Builder) -> [e] -> Builder
buildEntries title entryBuilder entries =
    unlinesB $ title : map entryBuilder entries

tabEntryBuilder :: TextShow s => (FilePath, s) -> Builder
tabEntryBuilder (fp, s) = showb s <> "\t" <> fromString fp