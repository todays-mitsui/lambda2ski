{-# LANGUAGE OverloadedStrings #-}

module Expr where

import Data.ByteString.Char8 (ByteString)
import Data.Map.Lazy (Map, empty, member, (!))


data Ident = Ident ByteString
             deriving (Eq, Ord, Show, Read)

infixl 9 :$
infixr 7 :^
data Expr = Expr :$ Expr
            | Ident :^ Expr
            | Var Ident
            deriving (Eq, Show, Read)

type Context = Map Ident Expr

emptyContext = empty

--------------------------------------------------------------------------------

i :: Expr
i = Var (Ident "i")

k :: Expr
k = Var (Ident "k")

s :: Expr
s = Var (Ident "s")

--------------------------------------------------------------------------------

-- | 式中の全てのλ抽象をSKコンビネーターに展開する
unlambda :: Expr -> Expr
unlambda e@(Var _) = e
unlambda (e :$ e') = unlambda e :$ unlambda e'
unlambda (x :^ e)  = unlambda $ resolve x e


-- | λ式の中から変数を取り除く
resolve :: Ident -> Expr -> Expr
resolve x e@(Var y)
  -- ^x.x === i
  | x == y                = i
  -- ^x.y === `ky
  | otherwise             = k :$ e

resolve x e@(e' :$ e''@(Var y))
  -- ^x.M === `kM
  | not (x `isFreeIn` e)  = k :$ e
  -- ^x.`Mx === M
  | not (x `isFreeIn` e') = e'
  -- ^x.`MN === ``s ^x.M ^x.N
  | otherwise             = s :$ resolve x e' :$ resolve x e''

resolve x e@(e' :$ e'')
  -- ^x.M === `kM
  | not (x `isFreeIn` e)  = k :$ e
  -- ^x.`M N === ``s ^x.M ^x.N
  | otherwise             = s :$ resolve x e' :$ resolve x e''

resolve x (y :^ e)        = resolve x $ resolve y e

--------------------------------------------------------------------------------

-- | 変数が式の中に自由変数として含まれるかどうか判定する
isFreeIn :: Ident -> Expr -> Bool
x `isFreeIn` (Var y)
  | x == y             = True
  | otherwise          = False
x `isFreeIn` (y :^ e)
  | x == y             = False
  | otherwise          = x `isFreeIn` e
x `isFreeIn` (e :$ e') = x `isFreeIn` e || x `isFreeIn` e'

--------------------------------------------------------------------------------

-- | 式に含まれる定義済み変数を展開する
subst :: Context -> Expr -> Expr
subst context x@(Var v)
  | v `member` context  = subst context $ context ! v
  | otherwise           = x
subst context (v :^ e)  = v :^ subst context e
subst context (e :$ e') = subst context e :$ subst context e'

--------------------------------------------------------------------------------

compile :: Context -> Expr -> Expr
compile context x@(Var v)
  | v `member` context      = compile context $ subst context x
  | otherwise               = x
compile context l@(_ :^ _)  = compile context $ unlambda l
compile context (e :$ e')   = compile context e :$ compile context e'
