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


data MoveSt = MoveSt {typeMove :: String, value :: Int}

moveSt :: MoveSt -> Pole -> Either Pole String
moveSt p (left, right)
	|(typeMove p)=="R"=landRight (value p) (left, right)
	|(typeMove p)=="L"=landLeft (value p) (left, right)
	|(typeMove p)=="U"=unlandAll
	|otherwise = Right "That was banana..."



readMoveSt :: String -> MoveSt
readMoveSt str = let (t:tail)=(words str) in (case1 t tail)
	where case1 t tail
		|(t/="B")&&(t/="U") = let (v:tail2)=tail in (MoveSt t (read v))
		|otherwise = MoveSt t 0

readMoves :: String -> [MoveSt]
readMoves str = map readMoveSt (lines str)

readMovesFromFile :: FilePath -> IO [MoveSt]
readMovesFromFile fname = readFile fname >>= (return . (map (\x -> readMoveSt x) ) . lines)

runMoves :: [MoveSt] -> Either Pole String
runMoves [] = Right "There is no move"
--runMoves l = foldr (\x p = ) (Left (0, 0)) l

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
landLeft n (left, right) = updatePole (left + n, right)

unlandAll :: Either Pole String
unlandAll = Left (0,0)

landRight :: Birds -> Pole -> Either Pole String
landRight n (left, right) = updatePole (left, right + n)


--banana :: Pole -> Either Pole String
--banana = Right "That was banana..."

{-
tests = all test [1..3]
  where
    test 1 = (return (0, 0) >>= landLeft 1 >>= landRight 4 
              >>= landLeft (-1) >>= landRight (-2)) == Right
    test 2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Just (2, 4)
    test 3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Nothing
-}
