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

operationsFromFile fname = readFile fname >>= (return . (map (divideString)) . lines)

main = getArgs >>= \(x:y:xs) -> operationsFromFile x >>= return . (mapAllOperations (read y))
