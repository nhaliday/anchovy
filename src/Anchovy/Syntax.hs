module Anchovy.Syntax where

type Name = String

data Op = Plus | Minus | Times | Divide deriving (Eq, Ord, Show)

data Expr =
	Float Double
	| BinOp Op Expr Expr
	| Var Name
	| Call Name [Expr]
	| Func Name [Name] Expr
	| Extern Name [Name]
	deriving (Eq, Ord, Show)
