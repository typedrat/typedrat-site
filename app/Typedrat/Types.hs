module Typedrat.Types (RatM, RatActionCtx) where

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Redis as R
import Lucid
import Web.Spock

type RatM a = SpockM (PG.Connection, R.Connection) () () a
type RatActionCtx a = SpockActionCtx () (PG.Connection, R.Connection) () () a
