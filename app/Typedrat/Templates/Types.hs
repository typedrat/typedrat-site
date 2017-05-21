{-# LANGUAGE ScopedTypeVariables, TypeInType, GADTs, TypeOperators, ConstraintKinds #-}
module Typedrat.Templates.Types (RatTemplate, ratT, Key(..), TemplateVar, (=:), TemplateVars(..), TVContains, (=?), askVar) where

import Control.Monad.Reader
import Data.Kind
import GHC.TypeLits
import Lucid
import Web.Spock.Action

import Typedrat.Types

--

type RatTemplate xs a = HtmlT (Reader (TemplateVars xs)) a

ratT :: TemplateVars xs -> RatTemplate xs () -> RatActionCtx ctx st ()
ratT vars template = do
        setHeader "Content-Type" "text/html; charset=utf-8"
        lazyBytes $ runReader (renderBST template) vars

--

data Key (k :: Symbol) = K deriving (Show, Eq)

data Idx where
    Zero :: Idx
    Succ :: Idx -> Idx

data SIdx (i :: Idx) where
    SZero :: SIdx Zero
    SSucc :: SIdx n -> SIdx (Succ n)

class ToSIdx a where
    toSIdx :: SIdx a

instance ToSIdx Zero where
    toSIdx = SZero

instance (ToSIdx n) => ToSIdx (Succ n) where
    toSIdx = SSucc toSIdx

--

data TemplateVar (k :: Symbol) v where
    TVar :: KnownSymbol k => Key k -> v -> TemplateVar k v

(=:) :: (KnownSymbol k) => Key k -> v -> TemplateVar k v
(=:) = TVar

instance (KnownSymbol k, Show v) => Show (TemplateVar k v) where
    show (TVar _ v) = concat ["\"", symbolVal (K :: Key k), "\" =: ", show v]

data TemplateVars xs where
    TVNil :: TemplateVars '[]
    (:::) :: KnownSymbol k => TemplateVars xs -> TemplateVar k v -> TemplateVars (TemplateVar k v ': xs)

infixl 7 :::

type family TVConcat (xs :: [Type]) (ys :: [Type]) :: [Type] where
    TVConcat xs '[] = xs
    TVConcat xs (y ': ys) = y ': TVConcat xs ys

tvconcat :: TemplateVars as -> TemplateVars bs -> TemplateVars (TVConcat as bs)
tvconcat xs TVNil = xs
tvconcat xs (ys ::: y) = (tvconcat xs ys) ::: y

instance Show (TemplateVars '[]) where
    showsPrec d TVNil =
        showParen (d > 10) $ showString "[]"

instance (Show (TemplateVars ts), Show t) => Show (TemplateVars (t ': ts)) where
    showsPrec d (a ::: as) =
        showParen (d > 5) $
           showsPrec 6 a .
           showString " ::: " .
           showsPrec 6 as

type family KeyToIdx (k :: Symbol) (xs :: [Type]) :: Idx where
    KeyToIdx k (TemplateVar k v ': ys) = Zero
    KeyToIdx k (x ': xs) = Succ (KeyToIdx k xs)

type KeyToType k xs = IdxToType (KeyToIdx k xs) xs

type family IdxToType (n :: Idx) (xs :: [Type]) :: Type where
    IdxToType Zero (TemplateVar k v ': xs) = v
    IdxToType (Succ n) (x ': xs) = IdxToType n xs

type TVContains n k v xs = (KnownSymbol k, ToSIdx n, KeyToIdx k xs ~ n, KeyToType k xs ~ v)

(=?) :: forall n k v xs . TVContains n k v xs => TemplateVars xs -> Key (k :: Symbol) -> v
tv =? _ = getVar' (toSIdx :: SIdx n) tv
    where
        getVar' :: SIdx m -> TemplateVars ys -> IdxToType m ys
        getVar'  SZero    (_ ::: (TVar _ x)) = x
        getVar' (SSucc n) (tv ::: _) = getVar' n tv

askVar :: (TVContains n k v xs, MonadReader (TemplateVars xs) m) => Key k -> m v
askVar k = asks (=? k)
