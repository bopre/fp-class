import System.Environment

{-
  Написать функцию, которая по заданному списку строк возвращает сумму длин всех строк.
-}

listLength :: [a]->Int
listLength [] = 0
listLength (x:xs) = 1 + (listLength xs)

totalLength :: [String] -> Int
totalLength = foldl (\x y -> x + (listLength y)) 0 

{-
  Написать функцию, которая по заданному символу и целому числу n строит список строк,
  содержащих 1, 2, ..., n повторений символа. Функция должна возвращать Nothing, если n=0.
-}

build1 :: Char -> Int -> Maybe [String]
build1 c n = if (n<=0) then Nothing else Just (makeList 1)
	where   makeList :: Int -> [String]
		makeList i
			|i==n=[take n (repeat c)]
			|otherwise=(take i (repeat c)):(makeList (i+1))
{-
  Написать функцию, аналогичную по возможностям функции build1, но возвращающую при этом
  значение Either String [String], в котором значение слева должно свидетельствовать об
  одной из следующих особых ситуаций: 
  (*) n=0;
  (*) n > 100;
  (*) Роспотребнадзор запрещает создавать строки из символа 'x'.
-}

build2 :: Char -> Int -> Either String [String]
build2 c n
	|n==0=Left "n=0"
	|n>100=Left "n > 100"
	|c=='x'=Left "Роспотребнадзор запрещает создавать строки из символа 'x'"
	|otherwise = Right (makeList 1)
	where   makeList :: Int -> [String]
		makeList i
			|i==n=[take n (repeat c)]
			|otherwise=(take i (repeat c)):(makeList (i+1))

{-
  Параметрами командной строки являются имя файла, символ, целое число.
  1) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и
     вывести общую длину строк, переданных программе в качестве аргументов командной строки.
  2) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и вывести общую
     длину строк, содержащихся в заданном текстовом файле (результат readFile должен быть
     предварительно преобразован к списку строк).
  3) Пользуясь функцией totalLength, подсчитать общую длину строк для значений в контекстах,
     сформированных функциями build1 и build2 (в решении следует пользоваться возможностями
     Maybe и Either String как функторов).
-}

main = do
  undefined
