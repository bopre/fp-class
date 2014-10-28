{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и сумму различных чисел, встречающихся
  в заданных текстовых файлах.
-}

import System.Environment
import Control.Monad
import qualified Data.Set as Set

readNumFile :: (Num a, Read a) => FilePath -> IO [a]
readNumFile fname = do
	content <- readFile fname
	let xs = map read $ concatMap words $ lines content
	return xs

nub_set :: Set.Set a -> Int
nub_set = Set.size

elems_set :: Set.Set a -> [a]
elems_set = Set.elems

concTwoList [] l2 = l2
concTwoList l1 [] = l1
concTwoList (x:xs) l2 = x :(concTwoList xs l2)

countElemsIntList [] = 0
countElemsIntList (x:xs) = x+(countElemsIntList xs)

concAllList [] = []
concAllList [x] = x
concAllList (x:xs) = concTwoList x (concAllList xs)

solve :: [[Int]] -> (Int, Int)
solve list1 = ((nub_set set1),(countElemsIntList(elems_set set1)))
	where set1 = Set.fromList (concAllList list1)

main = getArgs >>= mapM readNumFile >>= print.solve
