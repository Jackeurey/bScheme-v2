module Parser where

import Types
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef = 
  emptyDef  {Token.commentLine   = ";",
            Token.commentStart  = "{-",
            Token.commentEnd    = "-}",
            Token.identStart    = letter,
            Token.identLetter   = (oneOf "+-?><[]/\\" <|> alphaNum),
            Token.reservedNames = ["if", "quote", "define", "lambda", 
                                   "let", "else", "true", "false", "nil"],
            Token.reservedOpNames = ["+", "-", "id", "null?",
                                      "/","*","cons","car","cdr", "or", "and"]}

lexer      = Token.makeTokenParser languageDef -- Uses the languageDef to create a lexer
identifier = Token.identifier lexer            -- parse identifier
reserved   = Token.reserved lexer              -- parse a reserved name
parens     = Token.parens lexer                -- deals with paranthesis
integer    = Token.integer lexer
symbol     = Token.symbol lexer     
whiteSpace = Token.whiteSpace lexer
float      = Token.float lexer

-- Helper parsers ------------------------------------------------------------------------ 
nameList :: Parser [Name]
nameList = parens $ sepEndBy identifier whiteSpace

tryList :: [Parser a] -> Parser a
tryList ps = choice $ map (\p -> try p) ps

-- Grammer parsers -----------------------------------------------------------------------
parsePgrm :: Parser [Expr]
parsePgrm = many1 parseExpr 

parseExpr :: Parser Expr
parseExpr = tryList [apply, ifExpr, defVal, defFun, value, var]

apply :: Parser Expr
apply = parens $ Apply <$> parseExpr <*> (many parseExpr)

ifExpr :: Parser Expr
ifExpr = parens $ If <$> (reserved "if" *> parseExpr) <*> parseExpr <*> parseExpr 

defVal :: Parser Expr
defVal = parens $ DefVal <$> (reserved "define" *> identifier) <*> value

defFun :: Parser Expr
defFun = parens
  $ do reserved "define"
       (f:params) <- nameList
       body <- many1 parseExpr
       return $ DefFun f params body

value :: Parser Expr
value = tryList [sym, int, bool, str, lamb, list, nil, var] 
  where int  = (Value . Number) <$> integer
        bool = (Value . Boolean) True <$ reserved "true" <|>
               (Value . Boolean) False <$ reserved "false"
        sym  = (char '\'') >> (Value . Symbol) <$> identifier
        str  = (Value . Str) <$> (char '\"'  >> (manyTill anyChar  (char '\"')))
        lamb = parens (Value <$> (Lambda <$> (reserved "lambda" *> nameList) <*> parseExpr))
        list = (Value . List) <$> ((char '\'') *> parens (sepBy value whiteSpace))
        nil  = (Value Nil) <$ reserved "nil"

var :: Parser Expr
var = Var <$> identifier