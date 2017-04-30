module Typedrat.DB
    ( oQuery
    , runPostgres
    , runRedis
    , Hask, DbRead, DbWrite, Null, NotNull, Req, Opt, Field, TableField, Tableable(..)
    , PGSlug, pgSlug
    , BlogPost(..), PostId(..), blogPostTable, postQuery, pgBlogPost
    , Comment(..), CommentId(..), commentTable, commentQuery, pgComment, commentsForPost, numCommentsForPost
    , paginate
    ) where



import Typedrat.DB.Comment
import Typedrat.DB.Post
import Typedrat.DB.Slug
import Typedrat.DB.Types
import Typedrat.DB.Utils
import Typedrat.Types
