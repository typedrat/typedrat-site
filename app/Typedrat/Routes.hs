module Typedrat.Routes (postR, newPostR, renderPostUrl, postCommentR, oauthRedirectR, oauthCallbackR, logoutR, renderRoute) where

import qualified Data.Text as T
import Typedrat.DB
import Web.Routing.Combinators (PathState(..))
import Web.Slug
import Web.Spock

postR :: Path '[Slug] Open
postR = "posts" <//> var

newPostR :: Path '[] Open
newPostR = "posts" <//> "new"

renderPostUrl :: BlogPost Hask -> T.Text
renderPostUrl BlogPost{ _postSlug = s } = renderRoute postR s

postCommentR :: Path '[Slug] Open
postCommentR = "posts" <//> var <//> "comment"

oauthRedirectR :: Path '[] Open
oauthRedirectR = "auth" <//> "redirect"

oauthCallbackR :: Path '[] Open
oauthCallbackR = "auth" <//> "callback"

logoutR :: Path '[] Open
logoutR = "auth" <//> "logout"
