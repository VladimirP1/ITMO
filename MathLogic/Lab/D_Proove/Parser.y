{
module Parser where
import Lexer
import qualified Data.Map as Map
}
%name parse_
%tokentype { Token }
%error { parseError }

%token 
    var  { Var_ $$ }
    '!'  { Not_ }
    '&'  { And_ }
    '('  { BracketLeft_ }
    ')'  { BracketRight_ }
    '|'  { Or_ }
    '->' { Implies_ }
    '|-' { TurnStile_ }
    ','  { Comma_ }

%%

E : A { $1 } | Fs '|-' A { Proof $1 $3 [] }
Fs : {- empty -} { [] } | Fs ',' A { $1 ++ [$3] }
A : D { $1 } | D '->' A { Impl $1 $3 }
D : C { $1 } | D '|'  C { Or   $1 $3 }
C : T { $1 } | C '&'  T { And  $1 $3 }
T : '!' T { Not $2 } | '(' A ')' { $2 } | var { Var $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Node = Proof [Node] Node [Node] | Or Node Node | And Node Node | Impl Node Node | Not Node | Var String deriving Eq

instance Show Node where
    show (Proof a b c) = (foldr (\x y -> x ++ "," ++ y) "" (map show a)) ++ "|-" ++ (show b) ++ "\n" ++
        concatMap (\(i, l) -> show i ++ ". " ++ (show l) ++ "\n") (zip [1..] c)

    show (And x y) = pbin x y "&"
        where pbin x y s = "((" ++ (show x) ++ ")" ++ (s) ++ "(" ++ (show y) ++ "))"

    show (Or x y) = pbin x y "|"
        where pbin x y s = "((" ++ (show x) ++ ")" ++ (s) ++ "(" ++ (show y) ++ "))"

    show (Impl x y) = pbin x y "->"
        where pbin x y s = "((" ++ (show x) ++ ")" ++ (s) ++ "(" ++ (show y) ++ "))"

    show (Not n) = "(!(" ++ (show n) ++ "))"

    show (Var s) = s


instance Ord Node where
    (<=) (Impl _ _) (And _ _) = True
    (<=) (Impl _ _) (Or _ _) = True
    (<=) (Impl _ _) (Not _) = True
    (<=) (Impl _ _) (Var _) = True
    (<=) (And _ _) (Or _ _) = True
    (<=) (And _ _) (Not _) = True
    (<=) (And _ _) (Var _) = True
    (<=) (Or _ _) (Not _) = True
    (<=) (Or _ _) (Var _) = True
    (<=) (Not _) (Var _) = True
    (<=) (Impl x y) (Impl z w) = if x <= z then True else y <= w
    (<=) (And x y) (And z w) = if x <= z then True else y <= w 
    (<=) (Or x y) (Or z w) = if x <= z then True else y <= w 
    (<=) (Not x) (Not y) = x <= y
    (<=) (Var x) (Var y)  = x <= y 
    (<=) _ _ = False

parse = parse_ . alexScanTokens

}
