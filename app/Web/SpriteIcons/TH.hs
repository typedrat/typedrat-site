module Web.SpriteIcons.TH (spritesFromJSON, Octicon, toSVGAtSize, toSVGAtWidth, toSVGAtHeight, toSVG) where

import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Language.Haskell.TH
import Lucid

data Octicon = Octicon {
      _octName :: String
    , _octKeywords :: [String]
    , _octPath :: String
    , _octWidth :: Int
    , _octHeight :: Int
    }

instance FromJSON Octicon where
    parseJSON (Object o) = Octicon "" <$> o .: "keywords" <*> o .: "path" <*> (fromstr <$> o.: "width") <*> (fromstr <$> o .: "height")
        where fromstr (String t) = read (T.unpack t) :: Int

spritesFromJSON :: FilePath -> DecsQ
spritesFromJSON p = do
    d <- runIO $ BSL.readFile p
    let Just h = decode d :: Maybe (Map.Map String Octicon)
    Map.foldrWithKey' f (return []) h
    where
        t :: String -> String
        t ('-':b:c) = toUpper b : t c
        t (a:c) = a : t c
        t "" = ""
        f :: String -> Octicon -> DecsQ -> DecsQ
        f k Octicon{..} b = do
            ds <- b
            let n = VarP . mkName $ t k
            let kw = ListE $ LitE . StringL <$> _octKeywords
            let p = LitE $ StringL _octPath
            let w = LitE . IntegerL $ toInteger _octWidth
            let h = LitE . IntegerL $ toInteger _octHeight
            let v = NormalB $ foldl AppE (ConE 'Octicon) [LitE $ StringL k, kw, p, w, h]
            return (ValD n v [] : ds)

toSVGAtSize :: Octicon -> Float -> Float -> Html ()
toSVGAtSize Octicon{..} w h = svg_ [classes_ ["octicon",  "octicon-" `T.append` T.pack _octName], term "version" "1.1", width_ (T.pack $ show w), height_ (T.pack $ show h), term "aria-hidden" "true", term "viewBox" . T.pack $ unwords ["0 0", show _octWidth, show _octHeight]] $ toHtmlRaw _octPath

toSVGAtWidth :: Octicon -> Float -> Html ()
toSVGAtWidth o@Octicon{..} w = toSVGAtSize o w h
    where h = (fromIntegral _octHeight) * w / (fromIntegral _octWidth)

toSVGAtHeight :: Octicon -> Float -> Html ()
toSVGAtHeight o@Octicon{..} h = toSVGAtSize o w h
    where w = (fromIntegral _octWidth) * w / (fromIntegral _octHeight)

toSVG :: Octicon -> Html ()
toSVG o@Octicon{..} = toSVGAtSize o (fromIntegral _octWidth) (fromIntegral _octHeight)
