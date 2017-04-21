module Typedrat.Templates.Layout (layout) where

import qualified Data.Text as T
import Lucid

header :: T.Text -> T.Text -> Html ()
header username path = header_ . div_ [class_ "header"] $ do
    span_ [class_ "header-lambda"] "λ "
    span_ [class_ "header-username"] $ toHtml username
    span_ [class_ "header-at"] "@"
    "typedr.at"
    span_ [class_ "header-pathsep"] " : "
    toHtml path
    span_ [class_ "header-prompt"] " > "
    span_ [class_ "header-command"] "ls"

layout :: Html () -> Html ()
layout body = doctypehtml_ $ do
    head_ $ do
        link_ [rel_ "stylesheet", href_ "/static/out/css/all.css"]
        script_ [src_ "https://use.typekit.net/gex0hqe.js"] ""
        script_ "try{Typekit.load({ async: true });}catch(e){}"
        script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_CHTML"] ""
        title_ "guest@typedr.at : ~ >"

    body_ [class_ "light"] $ do
        header "guest" "~"

        div_ [class_ "body"] $
            aside_ $ do
                img_ [src_ "/static/img/headshot.png"]
                h1_ "Alexis Williams"
                p_ "Doing things, for some reason."
                body
