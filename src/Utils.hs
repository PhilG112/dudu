module Utils where

import AppRWST (MyApp)
import AppTypes (AppEnv (AppEnv, depth, fileStatus, path))
import Control.Monad.RWS (MonadIO (liftIO), MonadReader (ask, local), asks, RWST)
import Data.Foldable (traverse_)
import System.Directory (listDirectory)
import System.PosixCompat (FileStatus)
import System.FilePath ((</>))

currentPathStatus :: MyApp l s FileStatus
currentPathStatus = do
    AppEnv {fileStatus, path} <- ask
    liftIO $ fileStatus path

traverseDirectoryWith :: MyApp l s () -> MyApp l s ()
traverseDirectoryWith app = do
    curPath <- asks path
    content <- liftIO $ listDirectory curPath
    traverse_ go content
    -- asks path >>= liftIO . listDirectory >>= traverse_ go
    where
        go name = flip local app $
            \env -> env
                    { path = path env </> name
                    , depth = depth env + 1
                    }