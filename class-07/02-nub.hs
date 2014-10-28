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
nub_list listInt = countNotZero (fillList listInt builtEmptyList)

fillList :: [Int] -> [(Int,Int)] -> [(Int,Int)]
fillList [] list2 = list2
fillList (x:xs) list2 = fillList xs (incElemInList x list2)

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1+(listLength xs)

countNotZero :: [(Int,Int)] -> Int
countNotZero []=0
countNotZero ((k,x):xs) = if (x==0) then (countNotZero xs) else ((countNotZero xs)+1)

incElemInList :: Int -> [(Int,Int)] -> [(Int,Int)]
incElemInList _ [] = []
incElemInList n ((k,x):xs) = if (n==k) then ((k,(x+1)):xs) else ((k,x):(incElemInList n xs))

builtEmptyList :: [(Int,Int)]
builtEmptyList = [(i,0) | i <- [1..1000]]

nub_seq :: Seq.Seq a -> Int
nub_seq = undefined

getRealSize (x,y)=(y-x)+1

nub_arr :: Array Int Int -> Int
nub_arr arr = countNot0InArray 1000 (throughArray (getRealSize (bounds arr)) arr showarr1)

throughArray :: Int -> Array Int Int -> Array Int Int -> Array Int Int
throughArray 0 arr1 arr2 = arr2
throughArray n arr1 arr2 = throughArray (n-1) arr1 (incIElem (arr1 ! n) arr2)

showarr1 :: Array Int Int
showarr1 = array (1,1000) [(i,0) | i <- [1..1000]]

incIElem :: Int -> Array Int Int -> Array Int Int
incIElem n arr = array (1,1000) [(i,(if(i==n) then ((arr ! i)+1) else (arr ! i))) | i <- [1..1000]]

recv1 :: Int
recv1 = showarr1 ! 24

countNot0InArray :: Int -> Array Int Int -> Int
countNot0InArray 0 _ = 0
countNot0InArray n arr = (countNot0InArray (n-1) arr)+(if (arr ! n)==0 then 0 else 1)

main = do
  [fname] <- getArgs
  content <- readFile fname
  let xs = map read $ concatMap words $ lines content
  let (n:results) = [
        nub_set $ Set.fromList xs,
        nub_list xs,
        --nub_seq $ Seq.fromList xs,
	nub_list xs,
        nub_arr $ listArray (1,length xs) xs ]
  mapM_ print results
  when (any (/= n) results) $ putStrLn "Результаты не совпадают!"
