module Typedrat.DB.Post
    ( PostId(..)
    , BlogPost(..)
    , blogPostTable
    , pgBlogPost
    , postQuery
    , postWithSlug
    , renderPostBodyToHtml
    ) where

import Control.Arrow
import Data.Maybe
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

import Typedrat.DB.Slug
import Typedrat.DB.Types
import Typedrat.DB.Utils
import Typedrat.Types

--

newtype PostId a = PostId { _unPostId :: a }
                   deriving (Show, Eq, Functor)

$(makeAdaptorAndInstance' ''PostId)

data BlogPost a = BlogPost {
          _postId       :: PostId (TableField a Int PGInt4 NotNull Opt)
        , _postTitle    :: TableField a T.Text PGText NotNull Req
        , _postSlug     :: TableField a Slug PGSlug NotNull Req
        , _postTime     :: TableField a UTCTime PGTimestamptz NotNull Opt
        , _postBody     :: TableField a T.Text PGText NotNull Req
    }

deriving instance Show (BlogPost Hask)
deriving instance Eq (BlogPost Hask)

instance (Applicative (p (BlogPost a)), Profunctor p, ProductProfunctor p
    , Default p (TableField a Int PGInt4 NotNull Opt) (TableField b Int PGInt4 NotNull Opt)
    , Default p (TableField a T.Text PGText NotNull Req) (TableField b T.Text PGText NotNull Req)
    , Default p (TableField a Slug PGSlug NotNull Req) (TableField b Slug PGSlug NotNull Req)
    , Default p (TableField a UTCTime PGTimestamptz NotNull Opt) (TableField b UTCTime PGTimestamptz NotNull Opt)
    , Default p (TableField a T.Text PGText NotNull Req) (TableField b T.Text PGText NotNull Req)
    ) => Default p (BlogPost a) (BlogPost b) where
    def = BlogPost <$> dimap (_unPostId . _postId) PostId def
                   <*> lmap _postTitle def
                   <*> lmap _postSlug def
                   <*> lmap _postTime def
                   <*> lmap _postBody def

blogPostTable :: Table (BlogPost DbWrite) (BlogPost DbRead)
blogPostTable = Table "blog_posts" $ BlogPost
    <$> dimap (_unPostId . _postId) PostId (tableField "id")
    <*> lmap _postTitle (tableField "title")
    <*> lmap _postSlug (tableField "slug")
    <*> lmap _postTime (tableField "posted_at")
    <*> lmap _postBody (tableField "body")

pgBlogPost :: T.Text -> Slug -> T.Text -> BlogPost DbWrite
pgBlogPost title slug body = BlogPost (PostId Nothing) (pgStrictText title) (pgSlug slug) Nothing (pgStrictText body)

postQuery :: Query (BlogPost DbRead)
postQuery = queryTable blogPostTable

postWithSlug :: Slug -> RatActionCtx ctx st (Maybe (BlogPost Hask))
postWithSlug s = fmap listToMaybe . oQuery $ proc () -> do
    post@BlogPost{..} <- postQuery -< ()
    restrict -< _postSlug .=== pgSlug s
    returnA -< post


--

renderPostBodyToHtml :: (Monad m) => BlogPost Hask -> Either P.PandocError (HtmlT m ())
renderPostBodyToHtml = fmap (toHtmlRaw . P.writeHtmlString htmlOptions) .
                       P.readMarkdown markdownOpts . T.unpack . _postBody
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
