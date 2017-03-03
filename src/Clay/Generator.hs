{-# LANGUAGE OverloadedStrings #-}
module Clay.Generator (cssFile, renderToFile) where

import Clay.Render
import Clay.Stylesheet
import Language.Haskell.TH.Syntax
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.FilePath ((</>), (<.>), takeDirectory, takeExtension)
import System.Directory (getDirectoryContents, canonicalizePath)

cssFile :: String -> Q Exp
cssFile name = do
    loc <- qLocation
    runIO $ do
        srcFP <- canonicalizePath $ loc_filename loc
        mdir <- findProjectDir srcFP
        case mdir of
            Nothing -> error $ "Could not find .cabal file for path: " ++ srcFP
            Just dir -> return . LitE . StringL $ dir </> "static" </> "css" </> name <.> "css"
    where
        findProjectDir x = do
            let dir = takeDirectory x
            if dir == x
                then return Nothing
                else do
                    contents <- getDirectoryContents dir
                    if any isCabalFile contents
                        then return (Just dir)
                        else findProjectDir dir

        isCabalFile fp = takeExtension fp == ".cabal"

renderToFile :: FilePath -> Css -> IO ()
renderToFile path css = do
    putStrLn $ concat ["Generating CSS file '", path, "'"]
    T.writeFile path $ (T.tail $ render css) `mappend` "\n"
