module Typedrat.DB.Utils (paginate, oQuery, oDelete, runPostgres) where

import Data.Int
import Data.Profunctor.Product.Default
import qualified Database.PostgreSQL.Simple as PG
import qualified Opaleye as O
import qualified Web.Spock as S

import Typedrat.Types

paginate :: O.Order a -> Int -> Int -> O.Query a -> O.Query a
paginate o n p = O.limit n . O.offset ((p - 1) * n) . O.orderBy o

oQuery :: Default O.QueryRunner a b => O.Query a -> RatActionCtx ctx st [b]
oQuery = S.runQuery . flip O.runQuery

oDelete :: O.Table a b -> (b -> O.Column O.PGBool) -> RatActionCtx ctx st Int64
oDelete tbl f = S.runQuery $ \c -> O.runDelete c tbl f

runPostgres :: (PG.Connection -> IO a) -> RatActionCtx ctx st a
runPostgres = S.runQuery
