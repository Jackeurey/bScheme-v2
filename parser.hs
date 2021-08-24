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
            Token.reservedNames = ["if", "define", "lambda", 
                                   "else", "true", "false", "nil"],
            Token.reservedOpNames = ["+", "-", "null?", "quote", "unquote", "quasiquote",
                                      "eval", "apply", "/","*","cons","car","cdr", "or", "and"]}

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
parseExpr = tryList [apply, ifExpr, def, var, value]

apply :: Parser Expr
apply = parens $ Apply <$> parseExpr <*> (many parseExpr)

ifExpr :: Parser Expr
ifExpr = parens $ If <$> (reserved "if" *> parseExpr) <*> parseExpr <*> parseExpr 

def :: Parser Expr
def = parens $ Def <$> (reserved "define" *> identifier) <*> value

value :: Parser Expr
value = Value <$> tryList [sym, int, bool, str, lamb, list, nil]
  where int  = Number <$> integer
        bool = Boolean True <$ reserved "true" <|>
               Boolean False <$ reserved "false"
        sym  = Symbol <$> identifier
        str  = Str <$> (char '\"'  >> (manyTill anyChar  (char '\"')))
        lamb = parens (Lambda <$> (reserved "lambda" *> nameList) <*> parseExpr)
        list = parens (List <$> (parens $ sepEndBy parseExpr whiteSpace))
        nil  = Nil <$ reserved "nil"

var :: Parser Expr
var = Var <$> identifier