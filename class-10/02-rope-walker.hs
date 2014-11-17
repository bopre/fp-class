import Control.Monad 
{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       U
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться 
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}
import Control.Monad 
import System.Environment


data MoveSt = MoveSt {typeMove :: String, value1 :: Int, value2 :: Int}

moveSt :: MoveSt -> Pole -> Either Pole String
moveSt p (left, right)
	|(typeMove p)=="R"=landRight (value1 p) (left, right)
	|(typeMove p)=="L"=landLeft (value1 p) (left, right)
	|(typeMove p)=="U"=unlandAll
	|(typeMove p)=="LB"=landBoth (value1 p) (value2 p) (left, right)
	|otherwise = Right "That was banana..."



readMoveSt :: String -> MoveSt
readMoveSt str = let (t:tail)=(words str) in (case1 t tail)
	where case1 t tail
		|(t/="B")&&(t/="U") = let (v:tail2)=tail in (MoveSt t (read v) 0)
		|(t=="LB") = let (v1:v2:tail2)=tail in (MoveSt t (read v1) (read v2))
		|otherwise = MoveSt t 0 0

readMoves :: String -> [MoveSt]
readMoves str = map readMoveSt (lines str)

readMovesFromFile :: FilePath -> IO [MoveSt]
readMovesFromFile fname = readFile fname >>= (return . (map (\x -> readMoveSt x) ) . lines)

runMoves :: [MoveSt] -> Either Pole String
runMoves [] = Right "There is no move"
runMoves l = runThis l (Left (0, 0))
	where --runThis :: [MoveSt] -> Either Pole String -> Either Pole String
	      runThis _ (Right s) = Right s
	      runThis [x] (Left p) = moveSt x p
	      runThis (x:xs) (Left p) = runThis xs (moveSt x p)



main = readMovesFromFile "birds.txt" >>= return . runMoves


type Birds = Int

type Pole = (Birds, Birds)

balance = 3

updatePole :: Pole -> Either Pole String
updatePole p = if unbalanced p then Right "disbalanced" else Left p
  where
    unbalanced (l, r) = abs (l - r) > balance

landLeft :: Birds -> Pole -> Either Pole String
landLeft n (left, right) = addStr (updatePole (left + n, right))
	where addStr (Left p) = Left p
	      addStr (Right s) = Right (s++" left")

landBoth :: Birds -> Birds -> Pole -> Either Pole String
landBoth n1 n2 (left, right) = addStr (updatePole (left + n1, right + n2))
	where addStr (Left p) = Left p
	      addStr (Right s) = Right (if (n1>n2) then (s++" left") else (s++" right"))

unlandAll :: Either Pole String
unlandAll = Left (0,0)

landRight :: Birds -> Pole -> Either Pole String
landRight n (left, right) = addStr (updatePole (left, right + n))
	where addStr (Left p) = Left p
	      addStr (Right s) = Right (s++" right")
