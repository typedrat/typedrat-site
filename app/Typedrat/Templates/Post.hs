module Typedrat.Templates.Post (postTemplate) where

import Control.Monad
import qualified Data.Text as T
import Data.Time
import Lucid
import Typedrat.DB
import Typedrat.DB.Post
import Typedrat.Routes
import Typedrat.Templates.Types

postTemplate ::
    ( TVContains a "is_authenticated" Bool xs
    , TVContains b "post" (BlogPost Hask) xs
    , TVContains c "comments" [(Comment Hask, User Hask)] xs
    , TVContains d "user" (Maybe (User Hask)) xs
    ) => RatTemplate xs ()
postTemplate = article_ [class_ "post"] $ do
    auth <- askVar (K :: Key "is_authenticated")
    post@BlogPost{..} <- askVar (K :: Key "post")
    comments <- askVar (K :: Key "comments")

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
        forM_ comments $ article_ . \(c@Comment{..}, User{..}) -> do
                a_ [href_ _userProfile] $ do
                    img_ [src_ _userAvatar, class_ "comment-avatar"]
                    h2_ [class_ "comment-author"] $ toHtml _userName
                p_ [class_ "dateline"] . toHtml . formatTime defaultTimeLocale "%B %e, %Y" $ _commentTime
                let Right body = renderCommentBodyToHtml c
                body

        when auth $ do
            h2_ "Add Comment"
            form_ [method_ "post", action_ (renderPostUrl post `T.append` "/comment")] $ do
                textarea_ [name_ "body"] ""
                section_ [class_ "preview-area"] $ do
                    Just user <- askVar (K :: Key "user")
                    a_ [href_ $ _userProfile user] $ do
                        img_ [src_ $ _userAvatar user, class_ "comment-avatar"]
                        h2_ [class_ "comment-author"] . toHtml $ _userName user
                    section_ [class_ "markdown-preview"] ""

                section_ [class_ "controls"] $ do
                    aside_ $ do
                        "Supports "
                        a_ [href_ "http://pandoc.org/MANUAL.html#pandocs-markdown"] "Markdown with extensions"
                    button_ [type_ "submit"] "Post"
