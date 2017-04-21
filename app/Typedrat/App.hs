module Typedrat.App (app) where

import Database.PostgreSQL.Simple (Connection)
import qualified Network.Wai.Middleware.Static as S
import Typedrat.Views
import Typedrat.Routes
import Web.Spock

app :: SpockM Connection () () ()
app = do
    get root landing
    get postR postView
    get ("static" <//> wildcard) $ \_ -> respondMiddleware S.static
