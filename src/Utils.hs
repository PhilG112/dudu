module Utils where

import AppRWST (MyApp)
import AppTypes (AppEnv (AppEnv, depth, fileStatus, path))
import Control.Monad.RWS (MonadIO (liftIO), MonadReader (ask, local), RWST, asks)
import Data.Foldable (traverse_)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.PosixCompat (FileStatus)

currentPathStatus :: MyApp l s FileStatus
currentPathStatus = do
    AppEnv {fileStatus, path} <- ask
    liftIO $ fileStatus path

traverseDirectoryWith :: MyApp l s () -> MyApp l s ()
traverseDirectoryWith app = do
    curPath <- asks path
    content <- liftIO $ listDirectory curPath
    traverse_ go content
    where
        -- asks path >>= liftIO . listDirectory >>= traverse_ go

        go name = flip local app $
            \env ->
                env
                    { path = path env </> name
                    , depth = depth env + 1
                    }