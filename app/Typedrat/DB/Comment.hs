module Typedrat.DB.Comment (CommentId(..), Comment(..), pgComment, commentTable, commentQuery, renderCommentBodyToHtml, postWithComments, postsWithCommentNums) where

import Control.Arrow
import Control.Monad
import Data.Int
import Data.Time.Clock
import qualified Data.Text as T
import Data.Profunctor
import Data.Profunctor.Product
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH
import qualified Data.Set as S
import Lucid
import Opaleye
import qualified Text.Pandoc as P
import Web.Slug

import Typedrat.DB.Post
import Typedrat.DB.Slug
import Typedrat.DB.Types
import Typedrat.DB.User
import Typedrat.DB.Utils
import Typedrat.Types

newtype CommentId a = CommentId { _unCommentId :: a }
                    deriving (Show, Eq, Functor)

$(makeAdaptorAndInstance' ''CommentId)

data Comment a = Comment {
                 _commentId :: CommentId (TableField a Int PGInt4 NotNull Opt)
               , _commentAuthor :: UserId (TableField a Int PGInt4 NotNull Req)
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
                  <*> dimap (_unUserId . _commentAuthor) UserId def
                  <*> dimap (_unPostId . _commentPost) PostId def
                  <*> lmap _commentTime def
                  <*> lmap _commentBody def

commentTable :: Table (Comment DbWrite) (Comment DbRead)
commentTable = Table "comments" $ Comment
    <$> dimap (_unCommentId . _commentId) CommentId (tableField "id")
    <*> dimap (_unUserId . _commentAuthor) UserId (tableField "author_id")
    <*> dimap (_unPostId . _commentPost) PostId (tableField "post_id")
    <*> lmap _commentTime (tableField "posted_at")
    <*> lmap _commentBody (tableField "body")

commentQuery :: Query (Comment DbRead)
commentQuery = queryTable commentTable

pgComment :: UserId Int -> PostId Int -> T.Text -> Comment DbWrite
pgComment uid pid body = Comment (CommentId Nothing) (pgInt4 <$> uid) (pgInt4 <$> pid) Nothing (pgStrictText body)

commentsForPost :: BlogPost Hask -> Query (Comment DbRead, User DbRead)
commentsForPost BlogPost{..} = proc () -> do
    comment@Comment{..} <- commentQuery -< ()
    user@User{..} <- userQuery -< ()
    restrict -< _commentPost .=== (pgInt4 <$> _postId)
    restrict -< _userId .=== _commentAuthor
    returnA -< (comment, user)

numCommentsForPost :: BlogPost Hask -> Query (Column PGInt8)
numCommentsForPost BlogPost{..} = countRows $ proc () -> do
    comment@Comment{..} <- commentQuery -< ()
    restrict -< _commentPost .=== (pgInt4 <$> _postId)
    returnA -< comment

renderCommentBodyToHtml :: (Monad m) => Comment Hask -> Either P.PandocError (HtmlT m ())
renderCommentBodyToHtml = fmap (toHtmlRaw . P.writeHtmlString htmlOptions) .
                       P.readMarkdown markdownOpts . T.unpack . _commentBody
    where
        markdownOpts = P.def
            { P.readerParseRaw = True
            , P.readerSmart = True
            , P.readerExtensions = P.Ext_literate_haskell `S.insert` P.pandocExtensions
            }
        htmlOptions = P.def
            { P.writerHTMLMathMethod = P.MathJax ""
            , P.writerHighlight = True
            , P.writerHtml5 = True
            }


--

postWithComments :: Slug -> RatActionCtx ctx st (BlogPost Hask, [(Comment Hask, User Hask)])
postWithComments s = do
    (post:_) <- oQuery $ proc () -> do
            post@BlogPost{..} <- postQuery -< ()
            restrict -< _postSlug .=== pgSlug s
            returnA -< post
    comments <- oQuery $ commentsForPost post

    return (post, comments)

postsWithCommentNums :: Int -> RatActionCtx ctx st [(BlogPost Hask, Int64)]
postsWithCommentNums p = do
    posts <- oQuery $ paginate (desc _postTime) 10 p postQuery

    forM posts $ \p -> do
        n <- fmap head . oQuery $ numCommentsForPost p
        return (p, n)
