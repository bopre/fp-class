import System.Environment

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a
	|a `mod` 3==0 = 0
	|(a `mod` 3 /= 0)&&(a `mod` 2 /= 0)=a*a
	|otherwise = a*(a*a)

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF 0 f = f
reduceNF n f = reduceNF (n-1) (fmap reduce f)

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList l = foldl (\r x -> x:r) [] (foldl (\r (x,y) -> y:x:r) [] l)

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe [] = Nothing
toMaybe [(n,x)] = Just x
toMaybe ((n,x):xs) = let (nRes, xRes) =	foldl (\(n1,x1) (n2,x2) -> if (n1>n2) then (n1,x1) else (n2,x2)) (n,x) xs
			in Just xRes

toEither :: Integral a => [(a, a)]  -> Either String a
toEither [] = Left "Empty List"
toEither [(n,x)] = Right x
toEither ((n,x):xs) = let (nRes, xRes) =	foldl (\(n1,x1) (n2,x2) -> if (n1>n2) then (n1,x1) else (n2,x2)) (n,x) xs
			in Right xRes

-- воспользуйтесь в этой функции случайными числами
toIO :: Integral a => [(a, a)]  -> IO a
toIO = undefined

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs = undefined

readData :: FilePath -> IO [(Int, Int)]
readData = undefined
{-
main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  undefined
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print
-}


{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
-}
