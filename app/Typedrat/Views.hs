module Typedrat.Views (landing, postView) where

import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (Connection)
import qualified Network.Wai.Middleware.Static as S
import qualified Opaleye as O
import Typedrat.DB.Post
import Typedrat.Templates.Layout
import Typedrat.Templates.PostList
import Web.Slug
import Web.Spock
import Web.Spock.Lucid


landing :: SpockActionCtx () Connection () () ()
landing = do
    posts <- runQuery . flip O.runQuery $
        paginate (O.desc _postTime) 10 1 postQuery
    lucid $ layout (postList posts)

postView :: Integer -> Int -> Int -> Slug -> SpockActionCtx () Connection () () ()
postView _ _ _ _ = text ""
