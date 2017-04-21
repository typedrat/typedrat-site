{-# LANGUAGE TypeFamilyDependencies #-}
module Typedrat.DB.Utils (constantColumnUsing, Hask, DbRead, DbWrite, Null, NotNull, Req, Opt, Field, TableField, Tableable(..)) where

import Data.Profunctor
import Opaleye

constantColumnUsing :: Constant haskell (Column pgType)
                    -> (haskell' -> haskell)
                    -> Constant haskell' (Column pgType')
constantColumnUsing oldConstant f = dimap f unsafeCoerceColumn oldConstant

data Hask
data DbRead
data DbWrite

data Null
data NotNull

data Req
data Opt

type family Field f a b n where
    Field Hask   h o NotNull = h
    Field Hask   h o Null    = Maybe h
    Field DbRead h o NotNull = Column o
    Field DbRead h o Null    = Column (Nullable o)

type family TableField f a b n req where
    TableField Hask    h o n b   = Field Hask h o n
    TableField DbRead  h o n b   = Field DbRead h o n
    TableField DbWrite h o n Req = Field DbRead h o n
    TableField DbWrite h o n Opt = Maybe (Field DbRead h o n)

class Tableable a b | a -> b where
    tableField :: String -> TableProperties a b

instance Tableable (Column a) (Column a) where
    tableField = required

instance Tableable (Maybe (Column a)) (Column a) where
    tableField = optional
