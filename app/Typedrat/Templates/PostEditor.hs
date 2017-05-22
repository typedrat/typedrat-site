module Typedrat.Templates.PostEditor (postEditorTemplate) where

import Lucid

import Typedrat.DB
import Typedrat.Templates.Types

postEditorTemplate :: (TVContains c "user" (Maybe (User Hask)) xs) => RatTemplate xs ()
postEditorTemplate = article_ [id_ "new-post"] $  do
    form_ [method_ "post", action_ "/new_post"] $ do
        input_ [type_ "text", name_ "title", placeholder_ "Post Title"]
        textarea_ [name_ "body"] ""
        section_ [class_ "preview-area"] $ do
            Just user <- askVar (K :: Key "user")
            a_ [href_ $ _userProfile user] $ do
                img_ [src_ $ _userAvatar user, class_ "comment-avatar", width_ "48", height_ "48"]
                h2_ [class_ "comment-author"] . toHtml $ _userName user
            section_ [class_ "markdown-preview"] ""

        section_ [class_ "controls"] $ do
            aside_ $ do
                "Supports "
                a_ [href_ "http://pandoc.org/MANUAL.html#pandocs-markdown"] "Markdown with extensions"
            button_ [type_ "submit"] "Post"
