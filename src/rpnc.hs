-- Reimplementation of a simple RPN parser in Haskell.

import System.Environment
import System.IO
import Data.List
import Data.Char
import Data.Maybe

-- Behold, the grammar!
--
-- key:  pN = production N
--       e  = epsilon, basically this production is allowed to be empty
--
-- p0 ::= <p2> <p1>
-- p1 ::= '+' <p2> <p1>
--    ::= '-' <p2> <p1>
--    ::= e
-- p2 ::= <p4> <p3>
-- p3 ::= '*' <p4> <p3>
--    ::= '/' <p4> <p3>
--    ::= '%' <p4> <p3>
--    ::= '^' <p4> <p3>
--    ::= e
-- p4 ::= <p6> <p5>
-- p5 ::= 'min' <p6> <p5>
--    ::= 'max' <p6> <p5>
--    ::= e
-- p6 ::= '(' <p0> ')'
--    ::= <symbol>
--    ::= <number>

data AST = SymbolAST String
         | NumberAST Float
         | AddAST AST AST
         | SubtractAST AST AST
         | MultiplyAST AST AST
         | DivideAST AST AST
         | ModuloAST AST AST
         | PowerAST AST AST
         | MinAST AST AST
         | MaxAST AST AST
           deriving (Show)

data Token = PlusToken 
           | MinusToken 
           | StarToken 
           | SlashToken 
           | PercentToken 
           | CaretToken 
           | LeftParenToken 
           | RightParentToken 
           | MinToken 
           | MaxToken
           | EOSToken
           | SymbolToken String
           | NumberToken String
             deriving Show

readNumber :: String -> String -> (Token, Maybe String)
readNumber [] lexeme 
    | (last lexeme) == '.' = error $ lexeme ++ ": MALFORMED NUMBER"
    | otherwise            = (NumberToken lexeme, Nothing)

readNumber (c:cs) lexeme
    | isDigit c                                  = readNumber cs $ lexeme ++ (c:[])
    | c == '.' && (not $ "." `isInfixOf` lexeme) = readNumber cs $ lexeme ++ (c:[])
    | not $ isDigit c                            = (NumberToken lexeme, Just (c:cs))
    | otherwise                                  = error $ lexeme ++ ":  malformed number"

_tokenize :: Maybe String -> [Token]
_tokenize Nothing = [EOSToken]
_tokenize (Just cs) = tokenize cs

tokenize :: String -> [Token]
tokenize [] = [EOSToken]
tokenize ('-':cs) = MinusToken : tokenize(cs)
tokenize ('+':cs) = PlusToken : tokenize(cs)
tokenize ('*':cs) = StarToken : tokenize(cs)
tokenize ('/':cs) = SlashToken : tokenize(cs)
tokenize ('%':cs) = PercentToken : tokenize(cs)
tokenize ('^':cs) = CaretToken : tokenize(cs)
tokenize (c:cs)
    | isSpace c = tokenize cs
    | isDigit c = (fst num) : _tokenize(snd num)
    | otherwise = [SymbolToken [c]]
    where
        num = readNumber cs [c]


main = do
    args <- getArgs
    --ast = compile args
    putStrLn "asdf"
