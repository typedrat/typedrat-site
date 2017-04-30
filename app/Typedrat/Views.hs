{-# LANGUAGE RecordWildCards #-}
module Typedrat.Views (landing, postView) where

import Control.Arrow
import Control.Monad
import qualified Data.Text as T
import Data.Text.Encoding
import Lucid
import Network.Wai (rawPathInfo)
import qualified Opaleye as O
import Opaleye ((.===), (./==))
import Web.Slug
import Web.Spock.Action
import Web.Spock.Lucid

import Typedrat.DB
import Typedrat.Templates.Layout
import Typedrat.Templates.Post
import Typedrat.Templates.PostList
import Typedrat.Types

landing :: RatActionCtx ()
landing = do
    posts <- oQuery $ paginate (O.desc _postTime) 10 1 postQuery
    postsWithN <- forM posts $ \p -> do
        n <- fmap head . oQuery $ numCommentsForPost p
        return (p, n)

    lucid $ layout "~" "ls" "" (postList postsWithN)

postView :: Slug -> RatActionCtx ()
postView s = do
    (post:_) <- oQuery $ proc () -> do
        post@BlogPost{..} <- postQuery -< ()
        O.restrict -< _postSlug .=== pgSlug s
        returnA -< post
    comments <- oQuery $ commentsForPost post

    lucid $ layout "~/posts" (T.concat ["cat ", unSlug s, ".md"]) "" (postTemplate post comments)
