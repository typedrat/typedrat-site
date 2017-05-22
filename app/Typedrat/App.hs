module Typedrat.App (runApp) where

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Redis as R
import qualified Network.Wai.Middleware.Static as S
import Data.HVect
import Typedrat.Auth
import Typedrat.Types
import Typedrat.Routes
import Typedrat.Views
import Web.Spock
import Web.Spock.Config

initHook :: RatActionCtx () st (HVect '[])
initHook = return HNil

app :: RatM st ()
app = prehook initHook $ do
    get root landing

    get postR postView
    getpost previewMarkdownR previewMarkdownView

    get oauthCallbackR callbackView
    get oauthRedirectR redirectView

    get ("static" <//> wildcard) $ \_ -> respondMiddleware S.static

    prehook authHook $ do
        post postCommentR addCommentView

        get logoutR logoutView

        prehook adminHook $ do
            get newPostR postEditorView
            post newPostR addPostView

runApp :: IO ()
runApp = do
    let connection = do
            pg <- PG.connectPostgreSQL ""
            r <- R.connect $ R.defaultConnectInfo { R.connectMaxConnections = 1 } -- Let Spock handle pooling
            return (pg, r)
    let close (pg, r) = do
            R.runRedis r R.quit
            PG.close pg
    spockCfg <- defaultSpockCfg (RatSession Nothing Nothing) (PCConn $ ConnBuilder connection close (PoolCfg 3 10 15)) ()
    runSpock 4242 $ spock spockCfg app

