module Main where
import Axioms
import Parser
import Lexer
import Utils
import ModusPonens

import Data.Array
import System.IO (isEOF)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Char

parseAll :: [String] -> [Node]
parseAll = map (parse . alexScanTokens . toByteString)

toArray x = listArray (1, length x) x

data Annotation = Axiom !Int | Assumption !Int | ModusPonens !Int !Int deriving Show

data Expression = Implicative !ByteString.ByteString !ByteString.ByteString !(Maybe Int) | Normal !ByteString.ByteString !(Maybe Int) deriving Show

instance Eq Expression where
    x == y = (tbs' x) == (tbs' y)

getAxCode (Implicative _ _ a) = a
getAxCode (Normal _ a) = a

tbs' (Implicative a b c) = ByteString.concat [toByteString "((", a, toByteString ")->(", b, toByteString "))"]
tbs' (Normal a c) = a

compressExpression (NodeBinary '>' a b) = Implicative (tbs a) (tbs b) (matchAxiom (NodeBinary '>' a b))
compressExpression x = Normal (tbs x) (matchAxiom x)

parseExpression expr = (compressExpression (parse $ alexScanTokens expr))

getExpressionLineSpan input line spans
    | [] <- input = spans
    | otherwise = let
            (exp:input') = input
            spans' = Map.insert
                    (tbs' exp)
                        (
                        case Map.lookup (tbs' exp) spans of
                            Just (minLine, maxLine) -> (min minLine line, max maxLine line)
                            Nothing -> (line, line)
                        )
                    spans
        in
            getExpressionLineSpan input' (line + 1) spans'

getSpans input = getExpressionLineSpan input 1 (Map.empty)


splitModusPonens (Implicative a b c) = Just (a, tbs' (Implicative a b c), b)
splitModusPonens (Normal _ c) = Nothing

getModusPonens spans (eA,eAB,eB) = case (Map.lookup eA spans, Map.lookup eB spans, Map.lookup eAB spans) of
        (Just (minA, maxA), Just (minB, maxB), Just (minAB, maxAB)) -> 
            if minB > minAB && minB > minA 
                then Just (minA, minAB, minB)
                else Nothing
        otherwise -> Nothing

solveModusPonenses lineN exprsArr spans output
    | lineN > (snd $ bounds exprsArr) = output
    | otherwise = 
        let
            expr = exprsArr ! lineN
            r = (splitModusPonens expr) >>= getModusPonens spans
            output' = case r of
                (Just (a,ab,b)) -> Map.insert (tbs' (exprsArr ! b)) (ab, a) output
                otherwise -> output
        in
            solveModusPonenses (lineN + 1) exprsArr spans output'

testModusPonens modusPonenses x = 
    case Map.lookup (tbs' x) modusPonenses of
        Just (a,b) -> Just (ModusPonens a b)
        Nothing -> Nothing

testAssumption context x = 
    case Map.lookup (tbs' x) context of
        Just a -> Just (Assumption a)
        Nothing -> Nothing

testAxiom x =
    case (getAxCode x) of
        Just a -> Just (Axiom a)
        Nothing -> Nothing

finallyAnnotate lineN exprsArr tMP tASS tAX output
    | lineN > (snd $ bounds exprsArr) = output
    | otherwise = let expr = (exprsArr ! lineN) in
        case tAX expr of
            Just ann -> finallyAnnotate (lineN + 1) exprsArr tMP tASS tAX ([ann] ++ output)
            Nothing -> 
                case tASS expr of
                    Just ann -> finallyAnnotate (lineN + 1) exprsArr tMP tASS tAX ([ann] ++ output)
                    Nothing ->
                        case tMP expr of
                            Just ann -> finallyAnnotate (lineN + 1) exprsArr tMP tASS tAX ([ann] ++ output)
                            Nothing -> []


findProved array i target =
    let  
        (a,b) = bounds array
    in if (fst (array ! b)) == target then Just b else Nothing
--findProved array i target =
--    let  
--        (a,b) = bounds array
--    in if i > b then Nothing else if (fst(array!i) == target) then Just i else findProved array (i+1) target

myBFS array queue done depth 
  | (not $ null queue) = let
        x = head $ Set.toList queue
        done' = Set.insert x done
        queue' = Set.delete x queue
        queue'' = case (array!x) of
            (x,Axiom a) -> queue'
            (x,Assumption a) -> queue'
            (x,ModusPonens a b) -> Set.insert b (Set.insert a queue')
    in
        myBFS array queue'' done' (depth + 1)
  | otherwise = done

printAnnotatedMapped lineNumber mapping (bs, ann) 
    | (Axiom i) <- (ann) = putStrLn ("[" ++ (show $ mapz lineNumber) ++ ". Ax. sch. " ++ (show i) ++ "] " ++ (fromByteString $ tbs' bs))
    | (Assumption i) <- (ann) = putStrLn ("[" ++ (show $ mapz lineNumber) ++ ". Hypothesis " ++ (show i) ++ "] " ++ (fromByteString $ tbs' bs))
    | (ModusPonens i j) <- (ann) = putStrLn ("[" ++ (show $ mapz lineNumber) ++ ". M.P. " ++ (show $ mapz i) ++ ", " ++ (show $ mapz j) ++ "] " ++ (fromByteString $ tbs' bs))
  where mapz x = isInMap x mapping


main :: IO ()
main = do
    headerStr <- getLine
    let (contextStr, toBeProvedStr) = splitTurnStile headerStr
    let toBeProved = tbs $ (parse . alexScanTokens . toByteString) toBeProvedStr
    let contextList = map tbs $ parseAll $ splitBy (==',') contextStr
    let context = Map.fromList $ (zip contextList [1..])
    
    contents <- ByteString.getContents
    let exprs = map parseExpression (filter (/=toByteString []) (ByteString.split (BS.c2w '\n') contents))
    let exprsArr = toArray exprs

    let spans = getSpans exprs
    let modusPonenses = solveModusPonenses 1 exprsArr spans Map.empty
    let annotated = reverse (finallyAnnotate 1 exprsArr (testModusPonens modusPonenses) (testAssumption context) testAxiom [])
    
    if (not $ null annotated) then do
        let annotatedArray = toArray(zip exprs annotated)
        case findProved annotatedArray 1 (Normal toBeProved (Just 0)) of
            (Just toProveIdx) -> do
                let annFinal = myBFS annotatedArray (Set.fromList [toProveIdx]) (Set.empty) 0
                let used = Set.toAscList (myBFS annotatedArray (Set.fromList [toProveIdx]) (Set.empty) 0)
                let mapping = Map.fromList $ zip used [1..]
                let result = map (\x -> printAnnotatedMapped x mapping (annotatedArray!x)) used
                let result' = foldl (>>) (return ()) result
                putStrLn headerStr
                result'
            Nothing -> putStrLn "Proof is incorrect"
    else putStrLn "Proof is incorrect"

    
    
    {-putStrLn ( show exprs )
    putStrLn ( show spans )
    putStrLn ( show modusPonenses )
    putStrLn ( show annotated )
    putStrLn ( show annFinal )
    putStrLn ( show toProveIdx )-}

