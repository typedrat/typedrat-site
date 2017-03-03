{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Style.Header (HeaderTheme(..), defHeaderTheme, headerCss) where

import Clay
import Clay.Material.Colors
import Prelude hiding (div, span)

data HeaderTheme where
    HeaderTheme :: (Font a) => {
          headerBg :: Color
        , headerFg :: Color
        , headerAccent :: Color
        , headerFont :: a
        , headerHeight :: Size b
        } -> HeaderTheme

defHeaderTheme :: HeaderTheme
defHeaderTheme = HeaderTheme
    { headerBg = materialColor Pink Tone500
    , headerFg = materialColor Pink Tone200
    , headerAccent = materialColor Pink Tone900
    , headerFont = Required (em 2) Nothing ["clone-rounded-latin"] [monospace]
    , headerHeight = px 60
    }

headerCss :: HeaderTheme -> Css
headerCss HeaderTheme{..} = do
    body |> header ? do
        display flex
        alignItems center
        justifyContent center
        flexDirection column
        flexGrow 1

        height headerHeight
        paddingLeft (px 10)

        backgroundColor headerBg
        color headerFg
        font headerFont

        boxShadowWithSpread nil (px 2) (px 2) nil (setA 0.25 black)

    let logo = star # byClass "logo"

    logo ? do
        alignSelf flexStart
        "text-space-collapse" -: "discard"
        display flex

    logo |> span ? do
        color headerAccent
        sym2 margin nil (px 5)

    logo |> star # byClass "logo-at" ?
        sym margin nil

    let cmd = star # byId "logo-command"
    cmd ? do
        sym margin nil
        color headerFg
    cmd # focus ? outlineStyle none
    cmd # selection ? backgroundColor headerAccent

    keyframes "cursor"
        [ (0,   color transparent)
        , (50,  color headerFg)
        , (100, color transparent)
        ]

    logo |> star # byClass "logo-cursor" ? do
        sym margin nil

        animationName "cursor"
        animationDuration (sec 1)
        animationTimingFunction (stepsStop 1)
        animationIterationCount infinite
