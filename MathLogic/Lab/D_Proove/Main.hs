module Main where

import Axioms
import Parser
import Lexer
import Deduce
import ElementaryProofs

import Data.Array
import System.IO (isEOF)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List

import Control.Monad

calculate :: (Map.Map String Bool) -> Node -> Bool
calculate mapz (And a b) = (calculate mapz a) && (calculate mapz b)
calculate mapz (Or a b) = (calculate mapz a) || (calculate mapz b)
calculate mapz (Impl a b) = (not (calculate mapz a)) || (calculate mapz b)
calculate mapz (Not a) = not (calculate mapz a)
calculate mapz (Var a) = let (Just x) = Map.lookup a mapz in x

getVars :: Node -> Set.Set String
getVars (And a b) = Set.union (getVars a) (getVars b)
getVars (Or a b) = Set.union (getVars a) (getVars b)
getVars (Impl a b) = Set.union (getVars a) (getVars b)
getVars (Not a) = getVars a
getVars (Var a) = Set.fromList [a]


proveSubExprs :: (Map.Map String Bool) -> Node -> (Bool, [(String, Annotation)])
proveSubExprs mapz (And a b) =
    let 
        (a_val, a_prf) = proveSubExprs mapz a
        (b_val, b_prf) = proveSubExprs mapz b
        val = a_val && b_val
        prf = eproof' ("&", [a_val, b_val]) [show a, show b]
    in (val, a_prf ++ b_prf ++ prf)
    
proveSubExprs mapz (Impl a b) =
    let 
        (a_val, a_prf) = proveSubExprs mapz a
        (b_val, b_prf) = proveSubExprs mapz b
        val = not a_val || b_val
        prf = eproof' ("->", [a_val, b_val]) [show a, show b]
    in (val, a_prf ++ b_prf ++ prf)

proveSubExprs mapz (Or a b) =
    let 
        (a_val, a_prf) = proveSubExprs mapz a
        (b_val, b_prf) = proveSubExprs mapz b
        val = a_val || b_val
        prf = eproof' ("|", [a_val, b_val]) [show a, show b]
    in (val, a_prf ++ b_prf ++ prf)

proveSubExprs mapz (Not a) =
    let 
        (a_val, a_prf) = proveSubExprs mapz a
        val = not a_val
        prf = eproof' ("!", [a_val]) [show a]
    in (val, a_prf ++ prf)

proveSubExprs mapz (Var a) =
    let
       val = let (Just x) = Map.lookup a mapz in x
       prf = [(if val then a else "(!(" ++ a ++ "))", Axiom)]
    in (val, prf)

getSubject (lastLine1, _) (lastLine2, _) =
    let
        (    a, _:_:expr) = break (=='-') lastLine1
        (nega, _:_:expr2) = break (=='-') lastLine2
        (a', nega') = if length a < length nega then (a, nega) else (nega, a)
    in
        (a', nega', expr)

mergeProofs (a,nega,expr) proof1 proof2 =
    let
        proof' = proof1 ++ proof2 ++ (noThirdOption a expr)
    in
        proof'
        --putStrLn $ printProof proof'

getProofList fixedVars expr = 
    let
        vars = getVars expr
        freeVars = Set.difference vars (Map.keysSet fixedVars)
        values = replicateM (length freeVars) [True, False]
        values' = map (\x -> Map.union fixedVars (Map.fromList $ zip (Set.toList freeVars) x)) values
        proofs = map (\x -> snd (proveSubExprs x expr)) values'
    in
        (freeVars, proofs)
        --putStrLn $ show $ map printProof proofs

collapse (freeVars, proofs) =
    let 
        subj = head $ Set.toList freeVars
        freeVars' = Set.drop 1 freeVars
        (a, b) = halves proofs
        a' = map (deduce subj) a
        b' = map (deduce ("(!(" ++ subj ++ "))")) b
        pairs = zip a' b'
        pairs' = zip pairs a
        proofs' = map (\(x,y) -> mergeProofs (subj,("(!(" ++ subj ++ "))"), let (a,b) = last y in a) (fst x) (snd x)) pairs'
    in
        (freeVars', proofs')
  where 
    halves x = splitAt ((length x) `div` 2) x

collapseAll y@(freeVars, proofs) = if Set.null freeVars then head proofs else collapseAll (collapse y)

count x xs = (length . filter (== x)) xs
eqsatTrue x y = all (\(a,b) -> if a then a == b else True) (zip x y)

getAssumtions expr dir = 
    let 
        vars = getVars expr
        values = replicateM (length vars) [False, True]
        values' = map (\x -> (Map.fromList $ zip (Set.toList vars) x)) values
        results = map (\x -> calculate x expr) values'
        calc = Map.fromList (zip values results)
        ok = filter (\x-> all (\(arg,val) -> not (eqsatTrue x (map (==dir) arg)) || val) (Map.toList calc)) values
    in
        if null ok then
            Nothing
        else
            let
                best = minimumBy (\x y -> compare (count True x) (count True y)) ok
                best' = zip (Set.toList vars) best
                assums = map (\(var, take) -> (var, dir)) (filter (\(var, take) -> take) best')
            in
                Just (Map.fromList assums)

prove expr assumptions = collapseAll (getProofList assumptions expr)

printAssumptions x =
    intercalate "," (map (\(var,val) -> if val then var else ("!" ++ var)) x)

main :: IO ()
main = do
    --getLine >>= (putStrLn . show . parse)
    --putStrLn $ show (Map.fromList [("A", True)])

    line <- getLine
    let expr = parse line
    --let (val1, proof1) = proveSubExprs (Map.fromList [("A", True), ("B", True)]) expr
    --let (val2, proof2) = proveSubExprs (Map.fromList [("A", False), ("B", True)]) expr
    --let proof1' = deduce "A" proof1
    --let proof2' = deduce "(!(A))" proof2

    --mergeProofs proof1' proof2'

    --putStrLn $ show $ getAssumtions expr True
    --putStrLn $ show $ getAssumtions (Not expr) False
    
    let assums = getAssumtions expr True
    case assums of
        Just x ->
          do
            putStr $ printAssumptions (Map.toList x)
            putStrLn $ "|-" ++ (show expr)
            putStr $ printProof $ prove expr x
        Nothing -> 
           do
            let expr' = (Not expr)
            let assums' = getAssumtions expr' False
            case assums' of
                Just x -> 
                  do
                    putStr $ printAssumptions (Map.toList x)
                    putStrLn $ "|-" ++ (show expr')
                    putStr $ printProof $ prove expr' x
                Nothing -> putStrLn ":("

    --putStrLn $ printProof $ prove expr (Map.fromList [])

    

    --let prf = getProofList (Map.fromList []) expr
    --let prf' = collapseAll prf
    --putStrLn $ printProof prf'

    --let prf1 = collapse prf
    --putStrLn $ concatMap (\x -> (fst $ last x)++"\n") (snd prf1)

    --putStrLn $ show (fst prf)
    --putStrLn $ concatMap (\x->"------\n" ++ printProof x) (snd prf)


