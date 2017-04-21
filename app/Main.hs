module Main (main) where

import Configuration.Dotenv
import Database.PostgreSQL.Simple
import Web.Spock
import Web.Spock.Config

import Typedrat.App

main :: IO ()
main = do
    onMissingFile (loadFile False ".env") (return ())

    spockCfg <- defaultSpockCfg () (PCConn $ ConnBuilder (connectPostgreSQL "") close (PoolCfg 3 10 15)) ()
    runSpock 4242 $ spock spockCfg app
