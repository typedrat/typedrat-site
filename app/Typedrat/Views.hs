{-# LANGUAGE RecordWildCards #-}
module Typedrat.Views (landing, postView) where

import qualified Data.Text as T
import Web.Slug
import Web.Spock.Lucid

import Typedrat.DB
import Typedrat.Templates.Layout
import Typedrat.Templates.Post
import Typedrat.Templates.PostList
import Typedrat.Types

landing :: RatActionCtx ()
landing = do
    p <- postsWithCommentNums 1
    lucid $ layout "~" "ls" "" (postList p)

postView :: Slug -> RatActionCtx ()
postView s = do
    (post, comments) <- postWithComments s

    lucid $ layout "~/posts" (T.concat ["cat ", unSlug s, ".md"]) "" (postTemplate post comments)
