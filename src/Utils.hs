module Utils where

import AppRWST (MyApp)
import AppTypes
    ( AppConfig (extension, maxDepth),
      AppEnv (AppEnv, cfg, depth, fileStatus, path),
    )
import Control.Monad.RWS
    ( MonadIO (liftIO),
      MonadReader (ask, local),
      MonadWriter (tell),
      RWST,
      asks,
      when,
    )
import Data.Foldable (traverse_)
import System.Directory (listDirectory)
import System.Directory.Extra (listFiles)
import System.FilePath (isExtensionOf, (</>))
import System.PosixCompat (FileStatus, isDirectory)

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

checkExtension :: AppConfig -> FilePath -> Bool
checkExtension cfg fp = maybe True (\e -> e `isExtensionOf` fp) (extension cfg)

fileCount :: MyApp (FilePath, Int) s ()
fileCount = do
    AppEnv {..} <- ask
    fs <- currentPathStatus
    when (isDirectory fs && depth <= maxDepth cfg) $ do
        traverseDirectoryWith fileCount
        files <- liftIO $ listFiles path
        tell [(path, length $ filter (checkExtension cfg) files)]