{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Style.Layout where

import Control.Monad
import Clay
import Clay.Material.Colors
import qualified Data.Text as T
import Prelude hiding (div)

import Style.Header

data LayoutTheme where
    LayoutTheme :: (Font a) =>
        { layoutBg :: Color
        , layoutFg :: Color
        , layoutSecondary :: MaterialHue
        , layoutAccent :: MaterialHue
        , layoutFont :: a
        , layoutHeadingFont :: T.Text
        } -> LayoutTheme

defLayoutTheme :: LayoutTheme
defLayoutTheme = LayoutTheme
    { layoutBg = materialColor Grey Tone900
    , layoutFg = materialColor Grey Tone50
    , layoutSecondary = Pink
    , layoutAccent = LightGreen
    , layoutFont = Required (px 14) Nothing ["skolar-sans-latin"] [sansSerif]
    , layoutHeadingFont = "clone-rounded-latin"
    }

layoutCss :: LayoutTheme -> Css
layoutCss LayoutTheme{..} = do
    body ? do
        backgroundColor layoutBg
        color layoutFg
        font layoutFont

    let bodyDiv = star # byClass "body"

    bodyDiv ? do
        display flex
        sym padding (px 5)

    bodyDiv |> star ?
        sym margin (px 5)

    bodyDiv |> aside ?
        flexGrow 20

    bodyDiv |> section ?
        flexGrow 80

    forM_ [h1, h2, h3, h4, h5, h6] $ \h -> h ? do
        fontFamily [layoutHeadingFont] [monospace]
        fontWeight (weight 200)
