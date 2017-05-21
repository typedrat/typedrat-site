module Typedrat.DB.Utils (paginate, oQuery, runPostgres, runRedis) where

import Data.Int
import Data.Profunctor.Product.Default
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Redis as R
import qualified Opaleye as O
import qualified Web.Spock as S

import Typedrat.Types

paginate :: O.Order a -> Int -> Int -> O.Query a -> O.Query a
paginate o n p = O.limit n . O.offset ((p - 1) * n) . O.orderBy o

oQuery :: Default O.QueryRunner a b => O.Query a -> RatActionCtx ctx st [b]
oQuery = S.runQuery . flip (O.runQuery . fst)

oDelete :: O.Table a b -> (b -> O.Column O.PGBool) -> RatActionCtx ctx st Int64
oDelete tbl f = S.runQuery $ \c -> O.runDelete (fst c) tbl f

runPostgres :: (PG.Connection -> IO a) -> RatActionCtx ctx st a
runPostgres = S.runQuery . (. fst)

runRedis :: R.Redis a -> RatActionCtx ctx st a
runRedis = S.runQuery . flip (R.runRedis . snd)
