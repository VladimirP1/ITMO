{
module Parser where
import Lexer
import Utils
import qualified Data.ByteString.Lazy as ByteString
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

data NodeOp = Oand | Oor | Oimplies
data Node = NodeBinary !Char Node Node | NodeNot Node | NodeVar ByteString.ByteString deriving Eq


nodeOr      = NodeBinary '|'
nodeAnd     = NodeBinary '&'
nodeImplies = NodeBinary '>'

tbs::Node -> ByteString.ByteString
tbs = toByteString . show

instance Show Node where
    show (NodeBinary op n1 n2) = let {dop '>'="->"; dop x = [x]} in "((" ++ (show n1) ++ ")" ++ (dop op) ++ "(" ++ (show n2) ++ "))"
    show (NodeNot n) = "(!(" ++ (show n) ++ "))"
    show (NodeVar s) = fromByteString s
         



--main = do
--    let x = calc [Not, Var "x"]
--    putStrLn $ show x

}
