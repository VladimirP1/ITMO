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
  | length x == 0         = []
  | isPrefixOf "!"  x     = Not : cont x
  | isPrefixOf "("  x     = BracketLeft : cont x
  | isPrefixOf ")"  x     = BracketRight : cont x
  | isPrefixOf "&"  x     = And : cont x
  | isPrefixOf "|"  x     = Or : cont x
  | isPrefixOf "->" x     = Implies : (cont $ tail x)
  | isWhitespace $ head x = cont x
  | otherwise             = case cont x of
                                 (Var y) : z -> (Var (head x : y)) : z
                                 z           -> (Var (head x :[])) : z 
  where cont = tokenise . tail

