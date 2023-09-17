{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppRTWTST where

import AppTypes (AppConfig, AppEnv, initialEnv)
import Control.Monad.Reader
    ( MonadIO,
      MonadReader,
      ReaderT (ReaderT, runReaderT),
    )
import Control.Monad.State (MonadIO, MonadState, StateT, evalStateT)
import Control.Monad.Writer (MonadIO, MonadWriter, WriterT (runWriterT))

newtype MyApp logEntry state a = MyApp
    { runApp ::
        ReaderT
            AppEnv
            ( WriterT
                [logEntry]
                (StateT state IO)
            )
            a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader AppEnv
        , MonadWriter [logEntry]
        , MonadState state
        )

runMyApp :: MyApp logEntry state a -> AppConfig -> state -> IO (a, [logEntry])
runMyApp app config st =
    evalStateT (runWriterT (runReaderT (runApp app) (initialEnv config))) st