{-# LANGUAGE TypeOperators, LambdaCase #-}
module Typedrat.Auth (Authenticated, IsAdmin, userFromSession, authHook, adminHook, redirectView, callbackView, logoutView) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as BS
import Data.HVect (HVect(..), ListContains)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Types.Status
import Network.Wai (requestHeaderReferer)
import Network.Wreq as Wreq
import Opaleye
import System.Environment
import Web.Spock hiding (params, post, header)
import Web.Spock.Action (request)

import Typedrat.DB
import Typedrat.Types

data Authenticated = Authenticated
data IsAdmin = IsAdmin

-- Has the side effect of logging out invalid users
userFromSession :: RatActionCtx ctx st (Maybe (User Hask))
userFromSession = readSession >>= \(RatSession mToken _) ->
    case mToken of
        Just token -> userWithToken token
        Nothing -> return Nothing

verifyUser :: User Hask -> RatActionCtx ctx st Bool
verifyUser User{ _userAuthToken = Just token } = do
    clientId <-  liftIO $ getEnv "GITHUB_CLIENT_ID"
    clientSecret <- liftIO $ getEnv "GITHUB_CLIENT_SECRET"
    let url = concat ["https://api.github.com/applications/", clientId, "/tokens/", T.unpack token]
    let opts = defaults & auth ?~ basicAuth (BS.pack clientId) (BS.pack clientSecret)

    r <- liftIO $ getWith opts url

    if r & views responseStatus (== notFound404)
        then clearAuthToken token >> return False
        else return True
verifyUser User{ _userAuthToken = Nothing } = return False


authHook :: RatActionCtx (HVect xs) st (HVect (Authenticated ': xs))
authHook = do
    oldCtx <- getContext
    mUser <- userFromSession
--    valid <- maybe (return False) verifyUser mUser
    if isJust mUser -- && valid
        then return (Authenticated :&: oldCtx)
        else do
            modifySession (\rs -> rs { _githubAuth = Nothing })
            setStatus unauthorized401
            bytes "Authentication required."

adminHook :: ListContains n Authenticated xs => RatActionCtx (HVect xs) st (HVect (IsAdmin ': xs))
adminHook = do
    oldCtx <- getContext
    Just User { _userName = name } <- userFromSession
    if name == "typedrat"
        then return (IsAdmin :&: oldCtx)
        else do
            setStatus unauthorized401
            bytes "Administrator priviliges required."

redirectView :: RatActionCtx ctx st ()
redirectView = do
    clientId <-  liftIO $ getEnv "GITHUB_CLIENT_ID"
    ref <- fmap decodeUtf8 . requestHeaderReferer <$> request
    modifySession (\s -> s { _authRedirect = ref })
    redirect $ "https://github.com/login/oauth/authorize?client_id=" `T.append` (T.pack clientId)

callbackView :: RatActionCtx ctx st ()
callbackView = do
    clientId <-  liftIO $ getEnv "GITHUB_CLIENT_ID"
    clientSecret <- liftIO $ getEnv "GITHUB_CLIENT_SECRET"
    code <- param' "code"
    let url = "https://github.com/login/oauth/access_token"
    let opts = defaults
            & header "Accept" .~ ["application/json"]
    r <- liftIO . postWith opts url . toJSON . M.fromList $
            ([ ("client_id", clientId)
            , ("client_secret", clientSecret)
            , ("code", T.unpack code)
            ] :: [(String, String)])
    let tok = r ^. responseBody . key "access_token" . _String
    sess <- modifyReadSession (\s -> s { _githubAuth = Just tok })

    let url' = "https://api.github.com/user"
    let opts' = opts & auth ?~ oauth2Token (encodeUtf8 tok)
    user <- liftIO $ getWith opts' url'

    let Just id' = user ^? responseBody . key "id" . _Integral
    let name = user ^. responseBody . key "login" . _String
    let profile = user ^. responseBody . key "html_url" . _String
    let avatar = user ^. responseBody . key "avatar_url" . _String
    let dbUser = pgUser id' name (Just tok) profile avatar

    old <- userWithId $ UserId id'

    runPostgres $ \conn -> if isJust old
        then runUpdate conn userTable (const dbUser) (\User {_userId = oldId } -> oldId .=== UserId (pgInt4 id'))
        else runInsertMany conn userTable [dbUser]

    redirect . fromMaybe "/" . _authRedirect $ sess

logoutView :: (ListContains n Authenticated xs) => RatActionCtx (HVect xs) st ()
logoutView = do
    modifySession (\rs -> rs { _githubAuth = Nothing })
    redirect "/"
