module Typedrat.App (app) where

import qualified Network.Wai.Middleware.Static as S
import Typedrat.Types
import Typedrat.Routes
import Typedrat.Views
import Web.Spock

app :: RatM ()
app = do
    get root landing
    get postR postView
    get ("static" <//> wildcard) $ \_ -> respondMiddleware S.static
