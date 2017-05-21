module Main (main) where

import Configuration.Dotenv

import Typedrat.App

main :: IO ()
main = loadFile False ".env" >> runApp
