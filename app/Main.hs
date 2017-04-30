module Main (main) where

import Configuration.Dotenv
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Redis as R
import Web.Spock (runSpock, spock)
import Web.Spock.Config

import Typedrat.App

main :: IO ()
main = do
    loadFile False ".env"

    let connection = do
            pg <- PG.connectPostgreSQL ""
            r <- R.connect $ R.defaultConnectInfo { R.connectMaxConnections = 1 } -- Let Spock handle pooling
            return (pg, r)
    let close (pg, r) = do
            R.runRedis r R.quit
            PG.close pg
    spockCfg <- defaultSpockCfg () (PCConn $ ConnBuilder connection close (PoolCfg 3 10 15)) ()
    runSpock 4242 $ spock spockCfg app
