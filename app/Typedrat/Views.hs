{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Typedrat.Views (landing, postView, addCommentView) where

import Data.HVect
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Types.Status
import Opaleye
import Web.Slug
import Web.Spock.Action

import Typedrat.Auth
import Typedrat.DB
import Typedrat.Routes
import Typedrat.Templates.Layout
import Typedrat.Templates.Post
import Typedrat.Templates.PostList
import Typedrat.Templates.Types
import Typedrat.Types

usernameVar :: RatActionCtx ctx st (TemplateVars '[TemplateVar "is_authenticated" Bool, TemplateVar "username" T.Text])
usernameVar = do
    user <- userFromSession
    let username = case user of
            Just User{ _userName = name } -> name
            Nothing -> "guest"
    return $ TVNil
        ::: (K :: Key "username") =: username
        ::: (K :: Key "is_authenticated") =: isJust user

landing :: RatActionCtx ctx st ()
landing = do
    tv <- usernameVar
    posts <- postsWithCommentNums 1
    let tv' = tv
            ::: (K :: Key "path") =: "~"
            ::: (K :: Key "command") =: "ls"
            ::: (K :: Key "posts") =: posts

    ratT tv' $ layout "" postList

postView :: Slug -> RatActionCtx ctx st ()
postView s = do
    tv <- usernameVar
    (post, comments) <- postWithComments s
    let tv' = tv
            ::: (K :: Key "path") =: "~/posts"
            ::: (K :: Key "command") =: T.concat ["cat ", unSlug s, ".md"]
            ::: (K :: Key "post") =: post
            ::: (K :: Key "comments") =: comments

    ratT tv' $ layout "" postTemplate

addCommentView :: ListContains n Authenticated xs => Slug -> RatActionCtx (HVect xs) st ()
addCommentView s = postWithSlug s >>= \case
    Just post@BlogPost{ _postId = pid } -> do
        Just (User{ _userId = uid }) <- userFromSession
        body <- param' "body"
        runPostgres $ \conn -> runInsertMany conn commentTable [pgComment uid pid body]
        redirect (renderPostUrl post)
    Nothing -> setStatus internalServerError500
