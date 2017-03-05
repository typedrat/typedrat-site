module Typedrat.App where

import qualified Network.Wai.Middleware.Static as S
import Web.Spock
import Web.Spock.Lucid

import Typedrat.Templates.Layout

app :: SpockM () () () ()
app = do
    get root $ lucid testLayout
    get ("static" <//> wildcard) $ \_ -> respondMiddleware $ S.static
