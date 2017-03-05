module Typedrat.Templates.Layout where

import qualified Data.Text as T
import Lucid

header :: T.Text -> T.Text -> Html ()
header username path = header_ . div_ [class_ "header"] $ do
    span_ [class_ "header-lambda"] "λ "
    span_ [class_ "header-username"] $ toHtml username
    span_ [class_ "header-at"] "@"
    "typedr.at"
    span_ [class_ "header-pathsep"] " : "
    toHtml $ path
    span_ [class_ "header-prompt"] " > "
    span_ [class_ "header-command"] "ls"

layout :: Html () -> Html ()
layout = div_ [class_ "body"]

testLayout :: Html ()
testLayout = doctypehtml_ $ do
    head_ $ do
        link_ [rel_ "stylesheet", href_ "/static/out/css/all.css"]
        script_ [src_ "https://use.typekit.net/gex0hqe.js"] ""
        script_ "try{Typekit.load({ async: true });}catch(e){}"
        title_ "guest@typedr.at : ~ >"

    body_ [class_ "dark"] $ do
        header "guest" "~"

        layout $ do
            aside_ $ do
                img_ [src_ "https://static1.e621.net/data/f7/59/f75927244bd647eac0d0fec941790239.jpg"]
                h1_ "Alexis Williams"
                p_ "Doing things, for some reason."
