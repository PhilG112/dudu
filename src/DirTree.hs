module DirTree where

import AppRWST (MyApp)
import AppTypes (AppConfig (maxDepth), AppEnv (AppEnv, cfg, depth, path))
import Control.Monad.RWS (MonadReader (ask), MonadWriter (tell), when)
import Data.Text.Internal.Builder (Builder, fromString)
import System.FilePath
import System.PosixCompat (isDirectory)
import Utils (currentPathStatus, traverseDirectoryWith)

dirTree :: MyApp (FilePath, Int) s ()
dirTree = do
    AppEnv {..} <- ask
    fs <- currentPathStatus
    when (isDirectory fs && depth <= maxDepth cfg) $ do
        tell [(takeBaseName path, depth)]
        traverseDirectoryWith dirTree

treeEntryBuilder :: (FilePath, Int) -> Builder
treeEntryBuilder (fp, n) = fromString indent <> fromString fp
    where
        indent = replicate (2 * n) ' '