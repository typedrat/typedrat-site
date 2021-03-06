module Typedrat.Templates.PostList (postListTemplate) where

import Data.Int
import qualified Data.Text as T
import Data.Time
import Lucid
import Typedrat.DB
import Typedrat.DB.Post
import Typedrat.Routes

import Typedrat.Templates.Types

postListTemplate :: TVContains a "posts" [(BlogPost Hask, Int64)] xs => RatTemplate xs ()
postListTemplate = askVar (K :: Key "posts") >>= section_ [class_ "post-list"] . mapM_ (uncurry listItem)
    where
        listItem :: BlogPost Hask -> Int64 -> RatTemplate ys ()
        listItem post numComments = article_ [class_ "post"] $ do
            a_ [href_ (renderPostUrl post)] . h1_ . toHtml . _postTitle $ post
            p_ [class_ "post-dateline"] $ do
                toHtml . formatTime defaultTimeLocale "%B %e, %Y" . _postTime $ post
                " – "
                a_ [href_ $ renderPostUrl post `T.append` "#comments"] . toHtml $
                    (show numComments) ++ " comments"
            let Right body = renderPostBodyToHtml post
            section_ body
