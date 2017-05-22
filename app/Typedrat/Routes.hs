module Typedrat.Routes (postR, newPostR, renderPostUrl, previewMarkdownR, postCommentR, oauthRedirectR, oauthCallbackR, logoutR, renderRoute) where

import qualified Data.Text as T
import Typedrat.DB
import Web.Routing.Combinators (PathState(..))
import Web.Slug
import Web.Spock

postR :: Path '[Slug] Open
postR = "posts" <//> var

renderPostUrl :: BlogPost Hask -> T.Text
renderPostUrl BlogPost{ _postSlug = s } = renderRoute postR s

newPostR :: Path '[] Open
newPostR = "new_post"

previewMarkdownR :: Path '[] Open
previewMarkdownR = "preview_markdown"

postCommentR :: Path '[Slug] Open
postCommentR = "posts" <//> var <//> "comment"

oauthRedirectR :: Path '[] Open
oauthRedirectR = "auth" <//> "redirect"

oauthCallbackR :: Path '[] Open
oauthCallbackR = "auth" <//> "callback"

logoutR :: Path '[] Open
logoutR = "auth" <//> "logout"
