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
    
data Token = PlusToken 
           | MinusToken 
           | StarToken 
           | SlashToken 
           | PercentToken 
           | CaretToken 
           | LeftParenToken 
           | RightParenToken 
           | MinToken 
           | MaxToken
           | EOSToken
           | SymbolToken String
           | NumberToken String
             deriving (Show, Eq)

data Tree a = Leaf a 
            | Branch (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)

type AST = (Tree Token)



isEndOfToken :: Char -> Bool
isEndOfToken c = isSpace c || not (isAlphaNum c)

readNumber :: String -> String -> (Token, Maybe String)
readNumber [] lexeme 
    | (last lexeme) == '.' = error $ lexeme ++ ": MALFORMED NUMBER"
    | otherwise            = (NumberToken lexeme, Nothing)
readNumber (c:cs) lexeme
    | isDigit c                                  = readNumber cs $ lexeme ++ [c]
    | c == '.' && (not $ "." `isInfixOf` lexeme) = readNumber cs $ lexeme ++ [c]
    | not $ isDigit c                            = (NumberToken lexeme, Just (c:cs))
    | otherwise                                  = error $ lexeme ++ ":  malformed number"

readSymbol :: String -> String -> (Token, Maybe String)
readSymbol [] lexeme
    | lexeme == "min" = (MinToken, Nothing)
    | lexeme == "max" = (MaxToken, Nothing)
    | otherwise       = (SymbolToken lexeme, Nothing)
readSymbol (c:cs) lexeme 
    | lexeme == "min" && endtok = (MinToken, Just (c:cs))
    | lexeme == "max" && endtok = (MaxToken, Just (c:cs))
    | endtok                    = (SymbolToken lexeme, Just (c:cs))
    | otherwise                 = readSymbol cs $ lexeme ++ [c]
    where   
        endtok = isEndOfToken c

tokenize :: String -> [Token]
tokenize [] = [EOSToken]
tokenize cs = tokenizeM $ Just cs

tokenizeM :: Maybe String -> [Token]
tokenizeM Nothing = [EOSToken]
tokenizeM (Just ('-':cs)) = MinusToken      : tokenize(cs)
tokenizeM (Just ('+':cs)) = PlusToken       : tokenize(cs)
tokenizeM (Just ('*':cs)) = StarToken       : tokenize(cs)
tokenizeM (Just ('/':cs)) = SlashToken      : tokenize(cs)
tokenizeM (Just ('%':cs)) = PercentToken    : tokenize(cs)
tokenizeM (Just ('^':cs)) = CaretToken      : tokenize(cs)
tokenizeM (Just ('(':cs)) = LeftParenToken  : tokenize(cs)
tokenizeM (Just (')':cs)) = RightParenToken : tokenize(cs)
tokenizeM (Just (c:cs))
    | isSpace c = tokenize cs
    | isDigit c = (fst num) : tokenizeM (snd num)
    | otherwise = (fst sym) : tokenizeM (snd sym)
    where
        num = readNumber cs [c]
        sym = readSymbol cs [c] 

parse :: [Token] -> AST
parse tokens = fromJust $ fst $ p0 $ Just tokens

p0 :: Maybe [Token] -> (Maybe AST, Maybe [Token])
p0 Nothing = (Nothing, Nothing)
p0 (Just []) = (Nothing, Nothing)
p0 tokens = (Just (Branch l r), p1toks)
    where
        p2result = p2 tokens
        p1result = p1 p2toks
        l = fst p2result
        r = fst p1result
        p2toks = snd p2result
        p1toks = snd p1result

p1 :: Maybe [Token] -> (Maybe AST, Maybe [Token])
p1 Nothing = (Nothing, Nothing)
p1 (Just ([])) = (Nothing, Nothing)
        
p2 :: Maybe [Token] -> (Maybe AST, Maybe [Token])
p2 Nothing = (Nothing, Nothing)
p2 (Just ([])) = (Nothing, Nothing)

--p3 :: [Token] -> (Maybe (AST -> AST), Maybe [Token])
--p3 [] = (Nothing, Nothing)
--
--p4 :: [Token] -> (Maybe (AST -> AST), Maybe [Token])
--p4 [] = (Nothing, Nothing)
--
--p5 :: [Token] -> (Maybe (AST -> AST), Maybe [Token])
--p5 [] = (Nothing, Nothing)
--
p6 :: Maybe [Token] -> (Maybe AST, Maybe [Token])
p6 (Just (LeftParenToken:toks))
    | firstTok == RightParenToken = (ast,p0toks)
    | otherwise = error "Unbalanced parens"
    where
        (ast,p0toks) = p0 $ Just toks
        firstTok = head $ fromJust p0toks
p6 (Just ((SymbolToken (val)):toks)) = (Just $ Leaf $ SymbolToken val, Nothing)
p6 (Just ((NumberToken (val)):toks)) = (Just $ Leaf $ NumberToken val, Nothing)

--
--main = do
--    args <- getArgs
--    --ast = compile args
--    putStrLn "asdf"
