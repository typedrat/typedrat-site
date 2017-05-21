module Typedrat.Templates.PostList (postList) where

import Data.Int
import qualified Data.Text as T
import Data.Time
import Lucid
import Typedrat.DB
import Typedrat.Routes

import Typedrat.Templates.Types

postList :: TVContains a "posts" [(BlogPost Hask, Int64)] xs => RatTemplate xs ()
postList = askVar (K :: Key "posts") >>= ul_ [class_ "post-list"] . mapM_ (uncurry listItem)
    where
        listItem :: BlogPost Hask -> Int64 -> RatTemplate ys ()
        listItem post numComments = li_ $ do
            a_ [href_ (renderPostUrl post)] . h1_ . toHtml . _postTitle $ post
            p_ [class_ "post-dateline"] $ do
                toHtml . formatTime defaultTimeLocale "%B %e, %Y" . _postTime $ post
                " – "
                a_ [href_ $ renderPostUrl post `T.append` "#comments"] . toHtml $
                    (show numComments) ++ " comments"
