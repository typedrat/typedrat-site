module Typedrat.Templates.PostList (postList) where

import qualified Data.Text as T
import Data.Time
import Lucid
import Typedrat.DB.Post
import Typedrat.DB.Utils
import Typedrat.Routes

postList :: [BlogPost Hask] -> Html ()
postList = ul_ [class_ "post-list"] . mapM_ listItem
    where
        listItem :: BlogPost Hask -> Html ()
        listItem post = li_ $ do
            a_ [href_ (renderPostUrl post)] . h1_ . toHtml . _postTitle $ post
            aside_ $ do
                toHtml . formatTime defaultTimeLocale "%B %e, %Y" . _postTime $ post
                " – "
                a_ [href_ $ renderPostUrl post `T.append` "#comments"] . toHtml $
                    show (numCommentsForPost post) ++ " comments"
