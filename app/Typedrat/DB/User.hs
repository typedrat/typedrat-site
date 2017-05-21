module Typedrat.DB.User (UserId(..), User(..), pgUser, userTable, userQuery, userWithId, userWithToken, clearAuthToken) where

import Control.Arrow
import Data.Maybe
import Data.Profunctor
import Data.Profunctor.Product
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH
import qualified Data.Text as T
import Opaleye

import Typedrat.DB.Types
import Typedrat.DB.Utils
import Typedrat.Types

newtype UserId a = UserId { _unUserId :: a }
                   deriving (Show, Eq, Functor)

$(makeAdaptorAndInstance' ''UserId)

data User a = User {
                 _userId :: UserId (TableField a Int PGInt4 NotNull Req)
               , _userName :: TableField a T.Text PGText NotNull Req
               , _userAuthToken :: TableField a T.Text PGText Null Req
               , _userProfile :: TableField a T.Text PGText NotNull Req
               , _userAvatar :: TableField a T.Text PGText NotNull Req
    }

deriving instance Show (User Hask)
deriving instance Eq (User Hask)

instance (Applicative (p (User a)), Profunctor p, ProductProfunctor p
    , Default p (TableField a Int PGInt4 NotNull Req) (TableField b Int PGInt4 NotNull Req)
    , Default p (TableField a T.Text PGText NotNull Req) (TableField b T.Text PGText NotNull Req)
    , Default p (TableField a T.Text PGText Null Req) (TableField b T.Text PGText Null Req)
    ) => Default p (User a) (User b) where
    def = User <$> dimap (_unUserId . _userId) UserId def
               <*> lmap _userName def
               <*> lmap _userAuthToken def
               <*> lmap _userProfile def
               <*> lmap _userAvatar def

userTable :: Table (User DbWrite) (User DbRead)
userTable = Table "users" $ User
    <$> dimap (_unUserId . _userId) UserId (tableField "id")
    <*> lmap _userName (tableField "name")
    <*> lmap _userAuthToken (tableField "auth_token")
    <*> lmap _userProfile (tableField "profile_url")
    <*> lmap _userAvatar (tableField "avatar_url")

userQuery :: Query (User DbRead)
userQuery = queryTable userTable

pgUser :: Int -> T.Text -> Maybe T.Text -> T.Text -> T.Text -> User DbWrite
pgUser id' name token profile avatar = User (UserId $ pgInt4 id') (pgStrictText name) (maybeToNullable $ pgStrictText <$> token) (pgStrictText profile) (pgStrictText avatar)

userWithId :: UserId Int -> RatActionCtx ctx st (Maybe (User Hask))
userWithId id' = listToMaybe <$> oQuery (proc () -> do
        u@User{..} <- userQuery -< ()
        restrict -< _userId .=== (pgInt4 <$> id')
        returnA -< u)

userWithToken :: T.Text -> RatActionCtx ctx st (Maybe (User Hask))
userWithToken tok = listToMaybe <$> oQuery (proc () -> do
        u@User{..} <- userQuery -< ()
        restrict -< _userAuthToken .=== (toNullable $ pgStrictText tok)
        returnA -< u)

clearAuthToken :: T.Text -> RatActionCtx ctx st ()
clearAuthToken tok = runPostgres $ \conn ->
    (runUpdate conn userTable (\User{..} -> User _userId _userName Opaleye.null _userProfile _userAvatar) (\User{..} -> (toNullable $ pgStrictText tok) .=== _userAuthToken)) >> return ()
