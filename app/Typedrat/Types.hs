module Typedrat.Types (RatM, RatActionCtx, RatSession(..)) where

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import Web.Spock

type RatM st a = SpockM PG.Connection RatSession st a
type RatActionCtx ctx st a = SpockActionCtx ctx PG.Connection RatSession st a

data RatSession = RatSession { _githubAuth :: Maybe T.Text, _authRedirect :: Maybe T.Text } deriving (Show, Eq)
