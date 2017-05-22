{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Typedrat.Views (landing, postView, previewMarkdownView, postEditorView, addPostView, addCommentView) where

import Data.HVect
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Types.Status
import Opaleye
import Web.Slug
import Web.Spock.Action
import Web.Spock.Lucid

import Typedrat.Auth
import Typedrat.DB
import Typedrat.Markup
import Typedrat.Routes
import Typedrat.Templates
import Typedrat.Types

usernameVar :: RatActionCtx ctx st (TemplateVars '[TemplateVar "is_administrator" Bool, TemplateVar "is_authenticated" Bool, TemplateVar "user" (Maybe (User Hask))])
usernameVar = userFromSession >>= \case
    Just user -> return $ TVNil
        ::: (K :: Key "user") =: Just user
        ::: (K :: Key "is_authenticated") =: True
        ::: (K :: Key "is_administrator") =: (_userName user == "typedrat")
    Nothing -> return $ TVNil
        ::: (K :: Key "user") =: Nothing
        ::: (K :: Key "is_authenticated") =: False
        ::: (K :: Key "is_administrator") =: False

landing :: RatActionCtx ctx st ()
landing = do
    tv <- usernameVar
    posts <- postsWithCommentNums 1
    let tv' = tv
            ::: (K :: Key "path") =: "~"
            ::: (K :: Key "command") =: "ls"
            ::: (K :: Key "posts") =: posts

    ratT tv' $ layout "" postListTemplate

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

previewMarkdownView :: RatActionCtx ctx st ()
previewMarkdownView = do
    markdown <- body
    case renderMarkdown $ decodeUtf8 markdown of
        Left err -> do
            setStatus internalServerError500
            text . T.pack $ show err
        Right out -> do
            setHeader "Content-Type" "text/html; charset=utf-8"
            lucid out

postEditorView :: RatActionCtx ctx st ()
postEditorView = do
    tv <- usernameVar
    let tv' = tv
            ::: (K :: Key "path") =: "~/posts"
            ::: (K :: Key "command") =: "nano"

    ratT tv' $ layout "" postEditorTemplate

addPostView :: RatActionCtx ctx st ()
addPostView = do
    title <- param' "title"
    body <- param' "body" :: RatActionCtx ctx st T.Text
    let Just titleSlug = mkSlug title
    let pgPost = pgBlogPost title titleSlug body
    runPostgres $ \conn -> runInsertMany conn blogPostTable [pgPost]
    redirect $ renderRoute postR titleSlug

addCommentView :: ListContains n Authenticated xs => Slug -> RatActionCtx (HVect xs) st ()
addCommentView s = postWithSlug s >>= \case
    Just post@BlogPost{ _postId = pid } -> do
        Just (User{ _userId = uid }) <- userFromSession
        body <- T.filter (/= '\r') <$> param' "body" -- CR seems to break Lucid. :(
        runPostgres $ \conn -> runInsertMany conn commentTable [pgComment uid pid body]
        redirect (renderPostUrl post)
    Nothing -> setStatus internalServerError500
