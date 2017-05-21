module Typedrat.Templates.Layout (layout) where

import qualified Data.Text as T
import Lucid
import qualified Web.SpriteIcons as SI
import Web.SpriteIcons.TH (toSVG)

import Typedrat.Routes
import Typedrat.Templates.Types

layout :: (TVContains a "is_authenticated" Bool xs, TVContains b "username" T.Text xs, TVContains c "path" T.Text xs, TVContains d "command" T.Text xs) => RatTemplate xs () -> RatTemplate xs () -> RatTemplate xs ()
layout sidebar body = doctypehtml_ $ do
    auth <- askVar (K :: Key "is_authenticated")
    username <- askVar (K :: Key "username")
    path <- askVar (K :: Key "path")
    cmd <- askVar (K :: Key "command")

    head_ $ do
        link_ [rel_ "stylesheet", href_ "/static/out/all.css"]
        script_ [src_ "/static/out/all.js"] ""
        script_ [src_ "https://use.typekit.net/gex0hqe.js"] ""
        script_ "try{Typekit.load({ async: true });}catch(e){}"
        script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_CHTML"] ""
        title_ . toHtml . T.concat $ [username, "@typedr.at:", path, "> ", cmd] -- "guest@typedr.at : ~ >"

    body_ [class_ "light"] $ do
        header_ $ do
            section_ [class_ "header"] $ do
                span_ [id_ "header-toggle"] $ do
                    span_ [id_ "header-lambda"] "λ "
                    span_ [id_ "header-username"] $ toHtml username
                span_ [id_ "header-at"] "@"
                a_ [href_ "/"] "typedr.at"
                span_ [id_ "header-pathsep"] " : "
                toHtml path
                span_ [id_ "header-prompt"] " > "
                span_ [id_ "header-command"] $ toHtml cmd
            section_ [id_ "header-login"] $
                if not auth
                    then a_ [class_ "button", href_ $ renderRoute oauthRedirectR] $ do
                        toSVG SI.markGithub
                        "Log in with GitHub"
                    else a_ [class_ "button", href_ $ renderRoute logoutR] $ do
                         toSVG SI.markGithub
                         "Log out"
        div_ [class_ "body"] $ do
            aside_ $ do
                img_ [src_ "/static/img/headshot.png"]
                h1_ "Alexis Williams"
                p_ "Doing things, for some reason."

                sidebar
            body
