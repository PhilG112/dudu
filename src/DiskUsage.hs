module DiskUsage where

import AppRWST (MyApp)
import AppTypes (AppConfig (maxDepth), AppEnv (..))
import Control.Monad.RWS
    ( MonadReader (ask),
      MonadState (get),
      MonadWriter (tell),
      RWST,
      liftM2,
      modify,
      when,
    )
import System.Posix.Types (FileOffset)
import System.PosixCompat.Files
    ( FileStatus,
      fileSize,
      isDirectory,
      isRegularFile,
    )
import Utils
    ( checkExtension,
      currentPathStatus,
      traverseDirectoryWith,
    )

data DUEntryAction
    = TraverseDir {dirPath :: FilePath, requireReporting :: Bool}
    | RecordFileSize {fsize :: FileOffset}
    | None

diskUsage :: MyApp (FilePath, FileOffset) FileOffset ()
diskUsage = liftM2 decide ask currentPathStatus >>= processEntry
    where
        decide :: AppEnv -> FileStatus -> DUEntryAction
        decide AppEnv {..} fs
            | isDirectory fs =
                TraverseDir path (depth <= maxDepth cfg)
            | isRegularFile fs && checkExtension cfg path =
                RecordFileSize (fileSize fs)
            | otherwise = None

        processEntry ::
            DUEntryAction ->
            MyApp (FilePath, FileOffset) FileOffset ()
        processEntry TraverseDir {..} = do
            usageOnEntry <- get
            traverseDirectoryWith diskUsage
            when requireReporting $ do
                usageOnExit <- get
                tell [(dirPath, usageOnExit - usageOnEntry)]
        processEntry RecordFileSize {fsize} = modify (+ fsize)
        processEntry None = pure ()