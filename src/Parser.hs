module Parser where

import Text.Parsec
import Data.List (intercalate)
import Data.Functor.Identity

import Lambda
import Lexer

expression :: Parsec String u Expr
expression = foldl1 Application <$> (many1 term)

term :: Parsec String u Expr
term = parens expression
    <|> variable
    <|> literal
    <|> lambda

lambda :: Parsec String u Expr
lambda = flip (foldr Lambda) <$> (lambdaChar *> many1 identifier)
                             <*> (reservedOp "." *> expression)

literal :: Parsec String u Expr
literal = Literal <$> (LInt  <$> integer
                  <|> LBool <$> boolean
                  <|> LChar <$> character)

variable :: Parsec String u Expr
variable = Variable <$> identifier

-- * Util

lambdaChar :: Parsec String u ()
lambdaChar = reservedOp "\\" <|> reservedOp "λ" <|> reserved "lambda"

character :: Parsec String u Char
character = between (symbol "'") (symbol "'") anyChar
         <?> "character literal"

boolean :: Parsec String u Bool
boolean = fmap read (string "True" <|> string "False")

optionalParens :: Parsec String u a -> ParsecT String u Identity a
optionalParens p = parens p <|> p

