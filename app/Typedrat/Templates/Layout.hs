module Typedrat.Templates.Layout (layout) where

import qualified Data.Text as T
import Lucid
import Typedrat.Types
import qualified Web.SpriteIcons as SI
import Web.SpriteIcons.TH (toSVG, toSVGAtWidth, toSVGAtHeight, toSVGAtSize)

layout :: T.Text -> T.Text -> Html () -> Html () -> Html ()
layout path cmd sidebar body = doctypehtml_ $ do
    head_ $ do
        link_ [rel_ "stylesheet", href_ "/static/out/all.css"]
        script_ [src_ "/static/out/all.js"] ""
        script_ [src_ "https://use.typekit.net/gex0hqe.js"] ""
        script_ "try{Typekit.load({ async: true });}catch(e){}"
        script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_CHTML"] ""
        title_ . toHtml . T.concat $ ["guest@typedr.at:", path, "> ", cmd] -- "guest@typedr.at : ~ >"

    body_ [class_ "light"] $ do
        header_ $ do
            section_ [class_ "header"] $ do
                span_ [id_ "header-lambda"] "λ "
                a_ [href_ "/"] $ do
                    span_ [id_ "header-username"] "guest"
                    span_ [id_ "header-at"] "@"
                    "typedr.at"
                    span_ [id_ "header-pathsep"] " : "
                    toHtml path
                    span_ [id_ "header-prompt"] " > "
                    span_ [id_ "header-command"] $ toHtml cmd
            section_ [id_ "header-login"] $ do
                a_ [class_ "button", href_ "#"] $ do
                    toSVG SI.markGithub
                    "Log in with GitHub"
        div_ [class_ "body"] $ do
            aside_ $ do
                img_ [src_ "/static/img/headshot.png"]
                h1_ "Alexis Williams"
                p_ "Doing things, for some reason."

                sidebar
            body
