module Typedrat.Templates.Post (postTemplate) where

import Control.Monad
import qualified Data.Text as T
import Data.Time
import Lucid
import Typedrat.DB
import Typedrat.DB.Post
import Typedrat.Routes

postTemplate :: BlogPost Hask -> [Comment Hask] -> Html ()
postTemplate post@BlogPost{..} comments = article_ [class_ "post"] $ do
    a_ [href_ (renderPostUrl post)] . h1_ . toHtml $ _postTitle
    p_ [class_ "post-dateline"] $ do
        toHtml . formatTime defaultTimeLocale "%B %e, %Y" $ _postTime
        " – "
        a_ [href_ $ renderPostUrl post `T.append` "#comments"] . toHtml $
            show (length comments) ++ " comments"

    let Right body = renderPostBodyToHtml post
    article_ body

    section_ [id_ "comments"] $ do
        h1_ "Comments"
        forM_ comments $ article_ . \Comment{..} -> do
            p_ [class_ "post-dateline"] . toHtml . formatTime defaultTimeLocale "%B %e, %Y" $ _commentTime
            toHtml _commentBody
