{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и вывести различные числа, встречающиеся
  в каждом из заданных текстовых файлов. Указание: в решении следует воспользоваться множествами.
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

concAllList [] = []
concAllList [x] = x
concAllList (x:xs) = concTwoList x (concAllList xs)

solve :: (Num a, Ord a) => [[a]] -> (Int, [a])
solve list1 = ((nub_set set1),(elems_set set1))
	where set1 = Set.fromList (concAllList list1)
{-solve l1 = do
	let xs = concAllList l1
	let set1 = Set.fromList xs 
	let cnt = Set.size set1
	let elems1 = Set.elems set1
	let res = (cnt,elems1)
	return res
-}

main = getArgs >>= mapM readNumFile >>= print.solve
