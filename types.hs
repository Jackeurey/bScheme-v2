module Types where

import Data.List
import qualified Data.Map as M

type Env = M.Map Name LispValue
type Name = String

data LispValue = Number Integer
               | Boolean Bool
               | Symbol Name
               | Str String
               | Lambda [Name] Expr
               | List [Expr]
               | Nil

data Expr = Apply Expr [Expr]
          | If Expr Expr Expr
          | DefVal Name Expr
          | DefFun Name [Name] [Expr]
          | Value LispValue
          | Var Name  
          deriving (Eq, Show)



instance Show LispValue where
  show (Number x)      = show x
  show (Boolean x)     = if x then "true" else "false"
  show (Symbol x)      = show x
  show (Str x)         = show x
  show (Lambda _ _)    = "<procedure>" 
  show (List x)        = "(" ++ (intercalate " " .  map show) x ++ ")"
  show Nil             = "nil"

instance Eq LispValue where
  Number x == Number y   = x == y
  Boolean x == Boolean y = x == y
  Symbol x == Symbol y   = x == y
  Str x == Str y         = x == y
  List x == List y       = x == y
  Nil == Nil             = True 
  _ == _                 = False