{
module Parser where
import Tokeniser
}
%name parse
%tokentype { Token }
%error { parseError }

%token 
    var  { Var $$ }
    '!'  { Not }
    '&'  { And }
    '('  { BracketLeft }
    ')'  { BracketRight }
    '|'  { Or }
    '->' { Implies }

%%

E : D { $1 } | D '->' E { nodeImplies $1 $3 }
D : C { $1 } | D '|'  C { nodeOr      $1 $3 }
C : T { $1 } | C '&'  T { nodeAnd     $1 $3 }
T : '!' T { NodeNot $2 } | '(' E ')' { $2 } | var { NodeVar $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

--data Token = Var String | Not | And | Or | Implies | BracketLeft | BracketRight deriving Show

data Node = NodeBinary String Node Node | NodeNot Node | NodeVar String

nodeOr      = NodeBinary "|"
nodeAnd     = NodeBinary "&"
nodeImplies = NodeBinary "->"

instance Show Node where
    show (NodeBinary op n1 n2) = "(" ++ op ++ "," ++ (show n1) ++ "," ++ (show n2) ++ ")"
    show (NodeNot n) = "(!" ++ (show n) ++ ")"
    show (NodeVar s) = s


--main = do
--    let x = calc [Not, Var "x"]
--    putStrLn $ show x

}
