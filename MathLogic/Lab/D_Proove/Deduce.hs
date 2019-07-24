module Deduce where

import Axioms
import Parser
import Lexer

import Data.Array
import System.IO (isEOF)
import qualified Data.Set as Set
import qualified Data.Map as Map

data Annotation = Axiom | ModusPonens String  deriving Show

replace_vars map expr
    | [] <- expr = []
    | (x:xs) <- expr = 
        case Map.lookup x map of
            (Just y) -> y ++ (replace_vars map xs)
            Nothing -> (x : replace_vars map xs)

printProof proof = 
    concatMap (\(a, _) -> a ++ "\n") proof

replace_in_prf_line map (str, ModusPonens str2) = (replace_vars map str, ModusPonens $ replace_vars map str2)
replace_in_prf_line map (str, x) = (replace_vars map str, x)
replace_vars' mapz lst = 
    map (replace_in_prf_line mapz) lst

deduce hypo proof =
        concatMap deduce' proof
    where
        modpon x a b = 
            replace_vars'
                (Map.fromList [('a', a), ('b', b), ('x', x)])
                [("((x)->(a))->((x)->(a)->(b))->((x)->(b))", Axiom), ("((x)->(a)->(b))->((x)->(b))", ModusPonens "(x)->(a)"), ("(x)->(b)", ModusPonens "(x)->(a)->(b)")]
        axiom x a = 
            replace_vars'
                (Map.fromList [('a',a),('x',x)])
                [("a", Axiom),("(a)->(x)->(a)", Axiom),("(x)->(a)", ModusPonens "a")]
        self_proof x = 
            replace_vars'
                (Map.fromList [('a',x)])
                [
                    ("(a)->(a)->(a)",Axiom),
                    ("((a)->(a)->(a))->((a)->((a)->(a))->(a))->((a)->(a))",Axiom),
                    ("((a)->((a)->(a))->(a))->((a)->(a))",ModusPonens "(a)->(a)->(a)"),
                    ("((a)->((a)->(a))->(a))", Axiom),
                    ("(a)->(a)", ModusPonens "(a)->((a)->(a))->(a)")
                ]
        deduce' (line, ann) = 
            case ann of
                Axiom ->
                    if line == hypo then
                        self_proof hypo
                    else
                        axiom hypo line
                (ModusPonens j) -> modpon hypo j line
                    
                    
