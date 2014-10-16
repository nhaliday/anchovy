module Anchovy.Parser where

import qualified Anchovy.Lexer as L
import Anchovy.Syntax

import qualified Text.Parsec as P
import Text.Parsec (ParseError, (<|>))

import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

integer :: Parser Expr
integer = L.integer >>= (return . Float . fromInteger)

float :: Parser Expr
float = L.float >>= (return . Float)

var :: Parser Expr
var = L.identifier >>= (return . Var)

call :: Parser Expr
call = do
	name <- L.identifier
	args <- L.parens $ L.commaSep expr
	return $ Call name args

func :: Parser Expr
func = do
	P.try $ L.reserved "def"
	name <- L.identifier
	args <- L.parens $ P.many L.identifier
	body <- expr
	return $ Func name args body

extern :: Parser Expr
extern = do
	P.try $ L.reserved "extern"
	name <- L.identifier
	args <- L.parens $ P.many L.identifier
	return $ Extern name args

table =
	let left s f = Ex.Infix (L.reservedOp s >> return (BinOp f)) Ex.AssocLeft
	in [
		[left "*" Times, left "/" Divide]
		, [left "+" Plus, left "-" Minus]
	]

term :: Parser Expr
term = P.try float
	<|> P.try integer
	<|> P.try call
	<|> var
	<|> func
	<|> extern
	<|> L.parens expr

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

defn :: Parser Expr
defn = extern
	<|> func
	<|> expr

contents :: Parser a -> Parser a
contents p = do
	Tok.whiteSpace L.lexer
	r <- p
	P.eof
	return r

toplevel :: Parser [Expr]
toplevel = P.many $ do
	def <- defn
	L.reservedOp ";"
	return def

parseExpr :: String -> Either ParseError Expr
parseExpr = P.parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = P.parse (contents toplevel) "<stdin>"
 