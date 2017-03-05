module Main where

import Web.Spock
import Web.Spock.Config

import Typedrat.App

main :: IO ()
main = do
    spockCfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 $ spock spockCfg app
