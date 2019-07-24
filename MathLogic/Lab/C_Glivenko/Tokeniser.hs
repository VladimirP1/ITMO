module Tokeniser where
import Data.List

data Token = Not | BracketLeft | BracketRight | And | Or | Implies | Var String

instance Show Token where 
    show Not = "!"
    show BracketLeft = "("
    show BracketRight = ")"
    show And = "&"
    show Or = "|"
    show Implies = "->"
    show (Var x) = x

isWhitespace c = c == '\n' || c == '\r' || c == ' '

tokenise :: String -> [Token]
 
tokenise x 
  | null x                = []
  | head x == '('         = BracketLeft : cont x
  | head x == ')'         = BracketRight : cont x
  | head x == '!'         = Not : cont x
  | "->" `isPrefixOf` x   = Implies : cont (tail x)
  | head x == '&'         = And : cont x
  | head x == '|'         = Or : cont x
  | isWhitespace $ head x = cont x
  | otherwise             = case cont x of
                                 Var y : z -> Var (head x : y) : z
                                 z         -> Var [head x] : z 
  where cont = tokenise . tail

