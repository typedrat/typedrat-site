module Typedrat.Routes (postR, renderPostUrl) where

import Data.HVect (HVect(..))
import Data.Time
import qualified Data.Text as T
import Typedrat.DB
import Typedrat.DB
import Web.Routing.Combinators (PathState(..))
import Web.Slug
import Web.Spock

postR :: Path '[Slug] Open
postR = "posts" <//> var

renderPostUrl :: BlogPost Hask -> T.Text
renderPostUrl BlogPost{ _postSlug = s } = renderRoute postR s
