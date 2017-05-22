module Typedrat.Templates.Layout (layout) where

import Control.Monad
import qualified Data.Text as T
import Lucid
import qualified Web.SpriteIcons as SI
import Web.SpriteIcons.TH (toSVG)

import Typedrat.Routes
import Typedrat.DB
import Typedrat.Templates.Types

layout ::
    ( TVContains a "is_authenticated" Bool xs
    , TVContains b "is_administrator" Bool xs
    , TVContains c "user" (Maybe (User Hask)) xs
    , TVContains d "path" T.Text xs
    , TVContains e "command" T.Text xs
    ) => RatTemplate xs () -> RatTemplate xs () -> RatTemplate xs ()
layout sidebar body = doctypehtml_ $ do
    auth <- askVar (K :: Key "is_authenticated")
    admin <- askVar (K :: Key "is_administrator")
    user <- askVar (K :: Key "user")
    path <- askVar (K :: Key "path")
    cmd <- askVar (K :: Key "command")

    head_ $ do
        link_ [rel_ "apple-touch-icon-precomposed", sizes_ "57x57", href_ "https://typedr.at/static/img/apple-touch-icon-57x57.png"]
        link_ [rel_ "apple-touch-icon-precomposed", sizes_ "114x114", href_ "https://typedr.at/static/img/apple-touch-icon-114x114.png"]
        link_ [rel_ "apple-touch-icon-precomposed", sizes_ "72x72", href_ "https://typedr.at/static/img/apple-touch-icon-72x72.png"]
        link_ [rel_ "apple-touch-icon-precomposed", sizes_ "144x144", href_ "https://typedr.at/static/img/apple-touch-icon-144x144.png"]
        link_ [rel_ "apple-touch-icon-precomposed", sizes_ "120x120", href_ "https://typedr.at/static/img/apple-touch-icon-120x120.png"]
        link_ [rel_ "apple-touch-icon-precomposed", sizes_ "152x152", href_ "https://typedr.at/static/img/apple-touch-icon-152x152.png"]
        link_ [rel_ "icon", type_ "image/png", href_ "https://typedr.at/static/img/favicon-32x32.png", sizes_ "32x32"]
        link_ [rel_ "icon", type_ "image/png", href_ "https://typedr.at/static/img/favicon-16x16.png", sizes_ "16x16"]
        meta_ [name_ "application-name", content_ "typedr.at"]
        meta_ [name_ "msapplication-TileColor", content_ "#FBFBFB"]
        meta_ [name_ "msapplication-TileImage", content_ "https://typedr.at/static/img/mstile-144x144.png"]

        script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.7.1/katex.min.js", async_ "true"] ""
        script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.7.1/contrib/auto-render.min.js", async_ "true"] ""
        script_ [src_ "/static/out/highlight.pack.js", async_ "true"] ""
        script_ [src_ "/static/out/all.js", defer_ "true"] ""
        script_ "window.addEventListener('load', function () { hljs.initHighlighting(); renderMathInElement(document.body); });"

        link_ [rel_ "stylesheet", href_ "https://code.cdn.mozilla.net/fonts/fira.css"]
        link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.7.1/katex.min.css"]
        link_ [rel_ "stylesheet", href_ "/static/out/all.css"]

        title_ . toHtml . T.concat $ [maybe "guest" _userName user, "@typedr.at:", path, "> ", cmd] -- "guest@typedr.at : ~ >"

    body_ [class_ "light"] $ do
        header_ $ do
            section_ [class_ "header"] $ do
                span_ [id_ "header-toggle"] $ do
                    span_ [id_ "header-lambda"] "λ "
                    span_ [id_ "header-username"] . toHtml $ maybe "guest" _userName user
                span_ [id_ "header-at"] "@"
                a_ [href_ "/"] "typedr.at"
                span_ [id_ "header-pathsep"] " : "
                toHtml path
                span_ [id_ "header-prompt"] " > "
                span_ [id_ "header-command"] $ toHtml cmd
            section_ [id_ "header-login"] $ do
                when admin $
                    a_ [class_ "button", href_ $ renderRoute newPostR] "New post"

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
