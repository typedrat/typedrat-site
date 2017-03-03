{-# LANGUAGE TemplateHaskell #-}

module Main where

import Clay.Generator

import Style.Header
import Style.Layout

main = do
    renderToFile $(cssFile "header") $ headerCss defHeaderTheme
    renderToFile $(cssFile "layout") $ layoutCss defLayoutTheme
