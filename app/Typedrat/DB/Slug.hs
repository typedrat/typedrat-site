module Typedrat.DB.Slug (PGSlug, pgSlug) where

import Control.Exception
import Data.Maybe
import Data.Profunctor
import Data.Profunctor.Product.Default
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromField
import Opaleye
import Web.Slug

constantColumnUsing :: Constant haskell (Column pgType)
                    -> (haskell' -> haskell)
                    -> Constant haskell' (Column pgType')
constantColumnUsing oldConstant f = dimap f unsafeCoerceColumn oldConstant

data PGSlug

pgSlug :: Slug -> Column PGSlug
pgSlug = unsafeCoerceColumn . pgStrictText . unSlug

instance FromField Slug where
    fromField f mdata = do
        val <- fromField f mdata
        case parseSlug val of
            Right slug -> return slug
            Left e -> fail $ displayException e

instance QueryRunnerColumnDefault PGSlug Slug where
    queryRunnerColumnDefault = queryRunnerColumn (unsafeCoerceColumn :: Column PGSlug -> Column PGText) (fromJust . parseSlug) queryRunnerColumnDefault

instance Default Constant Slug (Column PGSlug) where
    def = constantColumnUsing (def :: Constant T.Text (Column PGText)) unSlug
