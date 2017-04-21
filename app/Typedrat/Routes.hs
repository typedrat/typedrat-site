module Typedrat.Routes (postR, renderPostUrl) where

import Data.HVect (HVect(..))
import Data.Time
import qualified Data.Text as T
import Typedrat.DB.Post
import Typedrat.DB.Utils
import Web.Routing.Combinators (PathState(..))
import Web.Slug
import Web.Spock

postR :: Path '[Integer, Int, Int, Slug] Open
postR = "posts" <//> var <//> var <//> var <//> var

renderPostUrl :: BlogPost Hask -> T.Text
renderPostUrl post = renderRoute postR y m d slug
    where
        (y, m, d) = toGregorian . utctDay . _postTime $ post
        slug = _postSlug post
