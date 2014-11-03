{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}
class Listable a where
   toList :: a -> [a]
   fromList :: [a] -> a

type StringDestr=String

instance Listable StringDestr where
   toList s = words s
   fromList [] = ""
   fromList (x:xs) = x++(fromList xs)

{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integral a - любое целое число разбивается на список цифр.
-}

type IntDestr=Int

instance Listable IntDestr where
   toList
   toList y = invert (step y)
	where step x
		|(x<0)=error "?"
		|(x<10)=[x]
		|otherwise=(x `mod` 10):(toList (x `div` 10))
	      invert [] = []
	      invert l = l
		
   fromList [] = 0
   fromList l = (\(x,k)->x) (iteration l)
	where   iteration [x] = (x,1) 
	        iteration (x:xs) = let (y,k) = (iteration xs)
	      			     in (((x*(k*10))+y),(k*10))

