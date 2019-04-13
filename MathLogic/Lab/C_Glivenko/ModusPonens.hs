module ModusPonens (main2, Result(..), Line(..), Annotation(..)) where
import Axioms
import Parser
import Tokeniser
import Utils

import Data.Array
import System.IO (isEOF)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString as ByteString
import Data.Char

parseAll :: [String] -> [Node]
parseAll = map (parse . tokenise)

data Annotation' = ModusPonens' !Int !Int | Axiom' !Int | Assumption' !Int deriving Show

data Cause = CauseFinal !ByteString.ByteString | CauseMB ![ByteString.ByteString] deriving Show

isCorrect stmtToInputLine line = isInMap line stmtToInputLine > 0
causeToList (CauseFinal x) = [x]
causeToList (CauseMB x) = x

resultToCauseUpd stmtToInputLine line resultToCause =
  let tbsline = tbs line in
    case line of
        (NodeBinary '>' x y) ->
          let {tbsX = tbs x;tbsY = tbs y} in
            case Map.lookup tbsY resultToCause of
                Just (CauseFinal z) -> resultToCause
                Just (CauseMB z) -> Map.insert tbsY (toInsert tbsX tbsY z) resultToCause
                Nothing -> Map.insert tbsY (toInsert tbsX tbsY []) resultToCause
          where toInsert x y old = if Map.member x stmtToInputLine then CauseFinal x else CauseMB (x : old)
        _ ->
            resultToCause

getLineNumber x stmtToInputLine | (Just y) <- Map.lookup x stmtToInputLine = y

resolveModusPonens' oK stmtToInputLine x = if oK then getLineNumber x stmtToInputLine else -1

readAndCheck context (curLine, inputProof, stmtToInputLine, resultToCause) =
  ifM isEOF
  (
    return (Just inputProof)
  ) --else
  ( do
    ln <- getLine
    case ln of
     [] -> return (Just inputProof)
     _ ->
      do
        let x = parse $ tokenise ln
        let xBS = tbs x

        let axiomN = detectAxiom x
        let contextN = isInMap xBS context

        -- Update reverse reasoner
        let resultToCause' = resultToCauseUpd stmtToInputLine x resultToCause

        -- Resolve modus ponens if we have to
        let sourceStmt = if axiomN >= 0 || contextN >= 0
            then []
            else filter (`Map.member` stmtToInputLine) (causeToList $ lookupFailOver xBS resultToCause (CauseMB []))
        let resultToCause'' = if null sourceStmt then resultToCause' else Map.insert xBS (CauseFinal $ head sourceStmt) resultToCause'
        --let resultToCause'' = resultToCause'

        let modusPonensOK = not (null sourceStmt)
        let modusPonensA = resolveModusPonens' modusPonensOK stmtToInputLine (head sourceStmt)
        let modusPonensB = resolveModusPonens' modusPonensOK stmtToInputLine $ toByteString ("((" ++ fromByteString (head sourceStmt) ++ ")->(" ++ fromByteString xBS ++ "))")

        -- Update state
        let lineOK = modusPonensOK || axiomN >= 0 || contextN >= 0
        let stmtToInputLine' = if lineOK && not (Map.member xBS stmtToInputLine) then Map.insert xBS curLine stmtToInputLine else stmtToInputLine
        let inputProof' = (xBS,if axiomN > 0 then Axiom' axiomN else if contextN > 0 then Assumption' contextN else ModusPonens' modusPonensA modusPonensB) : inputProof

        --putStrLn ((show resultToCause'') ++ " -- " ++ (show modusPonensA) ++ " " ++ (show modusPonensB))
        --putStrLn ((show curLine) ++ " " ++ (show ln) ++ "  @" ++ (show modusPonensA) ++ " " ++ (show modusPonensB))
        {-putStrLn ((show lineOK) ++ " | " ++ (show axiomN) ++ " " ++ (show contextN) ++ " " ++ (show modusPonensOK))
        putStrLn (show xBS)
        putStrLn (show stmtToInputLine)
        putStrLn (show resultToCause)
        putStrLn "--------------------------------"-}
        if lineOK then readAndCheck context (curLine + 1, inputProof', stmtToInputLine', resultToCause'') else return Nothing --putStrLn "Proof is incorrect"
  )


toArray x = listArray (1, length x) x


findProved array i target =
    let
        (a,b) = bounds array
    in if fst (array ! b) == target then Just b else Nothing

--simplify x = 0

myBFS array queue done depth
  | not $ null queue = let
      x = head $ Set.toList queue
      done' = Set.insert x done
      queue' = Set.delete x queue
      queue'' = case array!x of
          (x,Axiom' a) -> queue'
          (x,Assumption' a) -> queue'
          (x,ModusPonens' a b) -> Set.insert b (Set.insert a queue')
  in
      myBFS array queue'' done' (depth + 1)
  | otherwise = done

data Annotation = ModusPonens ByteString.ByteString ByteString.ByteString | Axiom !Int | Assumption deriving Show

data Line = Line { annotation :: Annotation, contnet :: ByteString.ByteString } deriving Show

data Result = ResultSuccess
    {
        context :: ByteString.ByteString,
        proved :: ByteString.ByteString,
        lines :: [Line]
    } | ResultError [Char] deriving Show

printAnnotatedMapped lineNumber mapping annotatedArray (bs, ann)
    | (Axiom' i) <- ann = Line (Axiom i) bs
    | (Assumption' i) <- ann = Line Assumption bs
    | (ModusPonens' i j) <- ann =  Line (ModusPonens (fst $ annotatedArray ! j) (fst $ annotatedArray ! i)) bs
  where mapz x = isInMap x mapping


main2 :: IO Result
main2 = do
    headerStr <- getLine
    let (contextStr, toBeProvedStr) = splitTurnStile headerStr
    let toBeProved = tbs $ (parse . tokenise) toBeProvedStr
    let contextList = map tbs $ parseAll $ splitBy (==',') contextStr
    let context = Map.fromList (zip contextList [1..])

    annotated <- readAndCheck context (1, [], Map.fromList[(toByteString "", -1)], Map.empty)

    case annotated of
         (Just ja) ->
          let annotatedArray = toArray $ reverse ja in
           case findProved annotatedArray 1 toBeProved of
             (Just toProveIdx) ->
              do
               let used = Set.toAscList (myBFS annotatedArray (Set.fromList [toProveIdx]) Set.empty 0)
               let mapping = Map.fromList $ zip used [1..]
               let result = map (\x -> printAnnotatedMapped x mapping annotatedArray (annotatedArray!x)) used
               --let result' = foldl (>>) (return ()) result
               --putStrLn $ show annotated
               --putStrLn $ show (findProved annotatedArray 1 toBeProved)
               --putStrLn $ show mapping
               return (ResultSuccess (toByteString contextStr) (toByteString toBeProvedStr) result)
             Nothing -> return $ ResultError "Proved expr is not the last line"
         Nothing -> return $ ResultError "?"
