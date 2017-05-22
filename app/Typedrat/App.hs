module Typedrat.App (runApp) where

import Control.Monad
import Control.Monad.IO.Class
import Data.HVect
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.Wai.Middleware.Static as S
import System.ReadEnvVar
import Web.Spock
import Web.Spock.Config

import Typedrat.Auth
import Typedrat.Types
import Typedrat.Routes
import Typedrat.Views

initHook :: RatActionCtx () st (HVect '[])
initHook = return HNil

app :: Bool -> RatM st ()
app staticFiles = prehook initHook $ do
    get root landing

    get postR postView
    getpost previewMarkdownR previewMarkdownView

    get oauthCallbackR callbackView
    get oauthRedirectR redirectView

    when staticFiles $ do
        strat <- liftIO $ S.initCaching S.PublicStaticCaching
        get ("static" <//> wildcard) $ \_ -> respondMiddleware (S.static' strat)

    prehook authHook $ do
        post postCommentR addCommentView

        get logoutR logoutView

        prehook adminHook $ do
            get newPostR postEditorView
            post newPostR addPostView

runApp :: IO ()
runApp = do
    let connection = PG.connectPostgreSQL ""
    let close = PG.close
    spockCfg <- defaultSpockCfg (RatSession Nothing Nothing) (PCConn $ ConnBuilder connection close (PoolCfg 3 10 15)) ()

    staticFiles <- readEnvVarDef "SERVE_STATIC" True
    runSpock 4242 $ spock spockCfg (app staticFiles)
