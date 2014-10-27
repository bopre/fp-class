{-
  Дан текстовый файл (его имя задано в параметрах командной строки), содержащий целые числа
  в диапазоне от 1 до 1000, разделённые пробелами и символами перевода строки. Определить
  количество различных чисел в нём, пользуясь для этого возможностями различных структур
  данных. 
-}

import Data.List
import qualified Data.Sequence as Seq
import qualified Data.IntSet as Set
import Data.Array.IArray
import System.Environment
import Control.Monad

nub_set :: Set.IntSet -> Int
nub_set = Set.size

nub_list :: [Int] -> Int
nub_list = undefined

nub_seq :: Seq.Seq a -> Int
nub_seq = undefined

nub_arr :: Array Int Int -> Int
nub_arr arr = 1000 - (count0InArray 1000 (throughArray 1000 showarr1 arr))

throughArray :: Int -> Array Int Int -> Array Int Int -> Array Int Int
throughArray 0 arr1 arr2 = arr1
throughArray n arr1 arr2 = throughArray (n-1) arr1 (incIElem (arr1 ! n) arr2)

showarr1 :: Array Int Int
showarr1 = array (1,1000) [(i,0) | i <- [1..1000]]

incIElem :: Int -> Array Int Int -> Array Int Int
incIElem n arr = array (1,1000) [(i,(if(i==n) then ((arr ! i)+1) else (arr ! i))) | i <- [1..1000]]

recv1 :: Int
recv1 = showarr1 ! 24

count0InArray :: Int -> Array Int Int -> Int
count0InArray 0 _ = 0
count0InArray n arr = (count0InArray (n-1) arr)+(if (arr ! n)==0 then 1 else 0)


main = do
  [fname] <- getArgs
  content <- readFile fname
  let xs = map read $ concatMap words $ lines content
  let (n:results) = [{-
        nub_set $ Set.fromList xs,
        nub_list xs,
        nub_seq $ Seq.fromList xs,-}
        nub_arr $ listArray (1,length xs) xs ]
  mapM_ print results
  let x = nub_arr $ listArray (1,length xs) xs
  print x
  when (any (/= n) results) $ putStrLn "Результаты не совпадают!"
