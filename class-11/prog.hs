import Control.Monad
import System.Environment

recvFucntionByName :: String -> BinarOpInt
recvFucntionByName "summand" = (+)
recvFucntionByName "multiplier" = (*)
recvFucntionByName "divisor" = (div)
recvFucntionByName _ = error "WRONG OPERATION!!!!!!!!!" -- (\x y -> (x+y)/0)

type Operat = (String, Int)
type BinarOpInt = (Int->Int->Int)


divideString :: String -> Operat
divideString [] = error "WRONG OPERATION!!!!!!!!!"
divideString (x:xs)
	|(x/='=')=let (oper, val) = (divideString xs) in (x:oper,val)
	|otherwise=([],(read xs))

mapAllOperations :: Int -> [Operat] -> Int
mapAllOperations value operList = foldl (\value (op,num) -> (recvFucntionByName op) num value) value operList

mapAllOperations1 :: [Operat] -> Int -> Int
mapAllOperations1 operList value = foldl (\value (op,num) -> (recvFucntionByName op) num value) value operList

operationsFromFile :: String -> IO [Operat]
operationsFromFile fname = readFile fname >>= (return . (map (divideString)) . lines)

readAllInt :: String -> IO [Int]
readAllInt fname = readFile fname >>= (return . (map (read)). words . (foldr (++) []) . lines)

mapOperationsToIntList :: [Int] -> [Operat] -> [Int]
mapOperationsToIntList l op = map (\x -> mapAllOperations x op) l

task11 :: String -> [Operat] -> IO [Int]
task11 fname op = readFile fname >>= (return . (map (mapAllOperations1 op)) . (map (read)). words . (foldr (++) []) . lines)

toPritableString :: [Int] -> String
toPritableString (z:zs) = foldl (\x y-> x ++ ('\n':(show y))) (show z) zs

main = getArgs >>= \(x:y:xs) -> operationsFromFile x >>= (task11 y) >>= return . toPritableString
