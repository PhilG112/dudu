module AppRWST where

import AppTypes (AppConfig, AppEnv, initialEnv)
import Control.Monad.RWS (RWST, evalRWST)

type MyApp logEntry state a = RWST AppEnv [logEntry] state IO a

runMyApp ::
    MyApp logEntry state a ->
    AppConfig ->
    state ->
    IO (a, [logEntry])
runMyApp app config s = evalRWST app (initialEnv config) s