module Typedrat.DB.Comment (CommentId(..), Comment(..), pgComment, commentTable, commentQuery, postWithComments, postsWithCommentNums) where

import Control.Arrow
import Control.Monad
import Data.Int
import Data.Time.Clock
import qualified Data.Text as T
import Data.Profunctor
import Data.Profunctor.Product
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH
import Opaleye
import Web.Slug

import Typedrat.DB.Post
import Typedrat.DB.Slug
import Typedrat.DB.Types
import Typedrat.DB.Utils
import Typedrat.Types

newtype CommentId a = CommentId { _unCommentId :: a }
                    deriving (Show, Eq, Functor)

$(makeAdaptorAndInstance' ''CommentId)

data Comment a = Comment {
                 _commentId :: CommentId (TableField a Int PGInt4 NotNull Opt)
               , _commentPost :: PostId (TableField a Int PGInt4 NotNull Req)
               , _commentTime :: TableField a UTCTime PGTimestamptz NotNull Opt
               , _commentBody :: TableField a T.Text PGText NotNull Req
    }

deriving instance Show (Comment Hask)
deriving instance Eq (Comment Hask)

instance (Applicative (p (Comment a)), Profunctor p, ProductProfunctor p
    , Default p (TableField a Int PGInt4 NotNull Opt) (TableField b Int PGInt4 NotNull Opt)
    , Default p (TableField a Int PGInt4 NotNull Req) (TableField b Int PGInt4 NotNull Req)
    , Default p (TableField a T.Text PGText NotNull Req) (TableField b T.Text PGText NotNull Req)
    , Default p (TableField a UTCTime PGTimestamptz NotNull Opt) (TableField b UTCTime PGTimestamptz NotNull Opt)
    ) => Default p (Comment a) (Comment b) where
    def = Comment <$> dimap (_unCommentId . _commentId) CommentId def
                  <*> dimap (_unPostId . _commentPost) PostId def
                  <*> lmap _commentTime def
                  <*> lmap _commentBody def

commentTable :: Table (Comment DbWrite) (Comment DbRead)
commentTable = Table "comments" $ Comment
    <$> dimap (_unCommentId . _commentId) CommentId (tableField "id")
    <*> dimap (_unPostId . _commentPost) PostId (tableField "post_id")
    <*> lmap _commentTime (tableField "posted_at")
    <*> lmap _commentBody (tableField "body")

commentQuery :: Query (Comment DbRead)
commentQuery = queryTable commentTable

pgComment :: PostId Int -> T.Text -> Comment DbWrite
pgComment pid body = Comment (CommentId Nothing) (pgInt4 <$> pid) Nothing (pgStrictText body)

commentsForPost :: BlogPost Hask -> Query (Comment DbRead)
commentsForPost BlogPost{..} = proc () -> do
    comment@Comment{..} <- commentQuery -< ()
    restrict -< _commentPost .=== (pgInt4 <$> _postId)
    returnA -< comment

numCommentsForPost :: BlogPost Hask -> Query (Column PGInt8)
numCommentsForPost BlogPost{..} = countRows $ proc () -> do
    comment@Comment{..} <- commentQuery -< ()
    restrict -< _commentPost .=== (pgInt4 <$> _postId)
    returnA -< comment

--

postWithComments :: Slug -> RatActionCtx (BlogPost Hask, [Comment Hask])
postWithComments s = do
    (post:_) <- oQuery $ proc () -> do
            post@BlogPost{..} <- postQuery -< ()
            restrict -< _postSlug .=== pgSlug s
            returnA -< post
    comments <- oQuery $ commentsForPost post

    return (post, comments)

postsWithCommentNums :: Int -> RatActionCtx [(BlogPost Hask, Int64)]
postsWithCommentNums p = do
    posts <- oQuery $ paginate (desc _postTime) 10 p postQuery

    forM posts $ \p -> do
        n <- fmap head . oQuery $ numCommentsForPost p
        return (p, n)
