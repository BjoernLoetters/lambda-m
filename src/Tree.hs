module Tree(Tree(..), Tree.parse, pretty) where

import Utility
import Text.ParserCombinators.Parsec
import Data.Bifunctor
import Control.Monad
import Data.List

data Tree = Var String
          | Ign
          | App Tree Tree
          | Abs Tree Tree
          | Num Integer
          | Chr Char
          | Tuple [Tree]
          | Match Tree [Tree]
          | Let [(Tree, Tree)] Tree
          | Data [Tree] Tree
          | Macro Tree Tree String
          deriving (Show, Eq)

pretty :: Tree -> String
pretty (Var name) = name
pretty Ign = "_"

pretty (Tuple values) = "(" ++ (intercalate ", " (fmap pretty values)) ++ ")"
pretty (Num num) = show num
pretty (Chr chr) = show chr
pretty (Let binders tree) = "let " ++ (intercalate ", " (fmap f binders)) ++ " in " ++ pretty tree where
    f :: (Tree, Tree) -> String
    f (left, right) = pretty left ++ " = " ++ pretty right
pretty (Data constructors tree) = "data " ++ (intercalate " | " (fmap pretty constructors)) ++ " in " ++ pretty tree
pretty (Macro left right content) = "macro " ++ pretty left ++ " = " ++ pretty right ++ " in " ++ content

pretty (Match value @ (App _ _) cases) = "(" ++ pretty value ++ ") match {" ++ (intercalate "" (fmap ((++) " case ") (fmap pretty cases))) ++ " }"
pretty (Match value @ (Abs _ _) cases) = "(" ++ pretty value ++ ") match {" ++ (intercalate "" (fmap ((++) " case ") (fmap pretty cases))) ++ " }"
pretty (Match value @ (Let _ _) cases) = "(" ++ pretty value ++ ") match {" ++ (intercalate "" (fmap ((++) " case ") (fmap pretty cases))) ++ " }"
pretty (Match value @ (Data _ _) cases) = "(" ++ pretty value ++ ") match {" ++ (intercalate "" (fmap ((++) " case ") (fmap pretty cases))) ++ " }"
pretty (Match value @ (Macro _ _ _) cases) = "(" ++ pretty value ++ ") match {" ++ (intercalate "" (fmap ((++) " case ") (fmap pretty cases))) ++ " }"
pretty (Match value cases) = pretty value ++ " match {" ++ (intercalate "" (fmap ((++) " case ") (fmap pretty cases))) ++ " }"

pretty (App left right @ (Abs _ _)) = pretty left ++ " (" ++ pretty right ++ ")"
pretty (App left @ (Abs _ _) right) = "(" ++ pretty left ++ ") " ++ pretty right
pretty (App left @ (Let _ _) right) = "(" ++ pretty left ++ ") " ++ pretty right
pretty (App left @ (Data _ _) right) = "(" ++ pretty left ++ ") " ++ pretty right
pretty (App left @ (Macro _ _ _) right) = "(" ++ pretty left ++ ") " ++ pretty right
pretty (App left right) = pretty left ++ " " ++ pretty right

pretty (Abs left @ (Abs _ _) right) = "(" ++ pretty left ++ ") => " ++ pretty right
pretty (Abs left @ (Let _ _) right) = "(" ++ pretty left ++ ") => " ++ pretty right
pretty (Abs left @ (Data _ _) right) = "(" ++ pretty left ++ ") => " ++ pretty right
pretty (Abs left @ (Macro _ _ _) right) = "(" ++ pretty left ++ ") => " ++ pretty right
pretty (Abs left right) = pretty left ++ " => " ++ pretty right

operatorSymbols :: [Char]
operatorSymbols = "+*~#-:.$%&/\\=?!^°<>|@"

keywords :: [String]
keywords = ["let", "in", "data", "case", "match", "=>", "=", "|"]

parse :: String -> Try Tree
parse input = first show $ Text.ParserCombinators.Parsec.parse parseTree "(unknown)" input

parseTree :: Parser Tree
parseTree = between spaces spaces (try parseMacro <|> try parseData <|> try parseLet <|> parseAbs False)

parseData :: Parser Tree
parseData = do
    parseKeyword "data"
    spaces
    constructors <- sepBy1 parseApp (between spaces spaces $ char '|')
    spaces
    parseKeyword "in"
    spaces
    tree <- parseTree
    return $ Data constructors tree

parseLet :: Parser Tree
parseLet = do
    parseKeyword "let"
    spaces
    let parseBinder = do
        binder <- parseApp
        spaces
        parseKeyword "="
        spaces
        expression <- parseTree
        return (binder, expression)
    binders <- sepBy1 parseBinder (between spaces spaces $ char ',')
    spaces
    parseKeyword "in"
    spaces
    tree <- parseTree
    return $ Let binders tree

parseMacro :: Parser Tree
parseMacro = do
    parseKeyword "macro"
    spaces
    binder <- parseApp
    spaces
    parseKeyword "="
    spaces
    expression <- parseTree
    spaces
    parseKeyword "in"
    spaces
    content <- many anyChar
    return $ Macro binder expression content

parseAbs :: Bool -> Parser Tree
parseAbs force = do
    left <- parseApp
    spaces
    let rhsParser = do
        parseKeyword "=>"
        spaces
        right <- parseTree
        return $ Abs left right
    if force
        then rhsParser
        else option left rhsParser

parseApp :: Parser Tree
parseApp = do
    let trySepBy1 parser separator = do
        head <- parser
        tail <- many $ try $ do
            separator
            parser
        return $ head : tail
    apps <- trySepBy1 parseValue spaces
    return $ foldl1 App apps

parseValue :: Parser Tree
parseValue = try $ do
    value <- choice [ try parseList, try parseString, try parseNum, try parseChr, try parseTuple, try parseIgnore, parseVar ]
    spaces
    result <- option value $ try $ do
        parseKeyword "match"
        spaces
        char '{'
        spaces
        let caseParser = do
            parseKeyword "case"
            spaces
            parseAbs True
        cases <- sepBy caseParser spaces
        spaces
        char '}'
        return $ Match value cases
    return result

parseIgnore :: Parser Tree
parseIgnore = do
    char '_'
    return Ign

parseNum :: Parser Tree
parseNum = do
    num <- many1 digit
    return $ Num (read num)

escape :: Bool -> Parser Char
escape stringMode = do
    char '\\'
    c <- if stringMode then oneOf "\\\"0nrvtbf" else oneOf "\\'0nrvtbf"
    return $ case c of
        '0' -> '\0'
        'n' -> '\n'
        'r' -> '\r'
        'v' -> '\v'
        't' -> '\t'
        'b' -> '\b'
        'f' -> '\f'
        c -> c

nonEscape :: Bool -> Parser Char
nonEscape True = noneOf "\\\"\0\n\r\v\t\b\f"
nonEscape False = noneOf "\\'\0\n\r\v\t\b\f"

character :: Bool -> Parser Char
character stringMode = (nonEscape stringMode) <|> (escape stringMode)

parseChr :: Parser Tree
parseChr = do
    char '\''
    c <- character False
    char '\''
    return $ Chr c

parseString :: Parser Tree
parseString = do
    char '"'
    string <- many $ character True
    char '"'
    return $ Data.List.foldr (\next acc -> App (App (Var "Cons") (Chr next)) acc) (Var "Nil") string

parseList :: Parser Tree
parseList = do
    char '['
    values <- sepBy parseTree (char ',')
    char ']'
    return $ Data.List.foldr (\next acc -> App (App (Var "Cons") next) acc) (Var "Nil") values

parseVar :: Parser Tree
parseVar = fmap Var (try parseAlphaNumericIdentifier <|> parseSymbolicIdentifier)

parseTuple :: Parser Tree
parseTuple = do
    values <- between (char '(') (char ')') $ do
        spaces
        result <- sepBy parseTree (char ',')
        spaces
        return result
    return $ Tuple values

parseIdentifier :: Parser Char -> Parser String
parseIdentifier symbols = try $ do
    let identifierParser = try $ do
        head <- symbols
        tail <- many (try symbols <|> digit)
        primes <- many $ char '\''
        return $ (head : tail) ++ primes
    identifier <- try $ lookAhead identifierParser
    if identifier `elem` keywords
            then unexpected ("'" ++ identifier ++ "' is a reserved keyword")
            else identifierParser

parseAlphaNumericIdentifier :: Parser String
parseAlphaNumericIdentifier = parseIdentifier letter

parseSymbolicIdentifier :: Parser String
parseSymbolicIdentifier = parseIdentifier (oneOf operatorSymbols)

parseKeyword :: String -> Parser ()
parseKeyword keyword = do
    string keyword
    notFollowedBy (choice [ try alphaNum, try (oneOf operatorSymbols), char '\'' ])
