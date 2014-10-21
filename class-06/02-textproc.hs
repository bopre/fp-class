{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}

import System.Environment
import System.Directory
import Data.Char



cntStr :: String -> Integer
cntStr []=0
cntStr (x:xs)
	|x=='\n'=1+cntStr xs
	|otherwise=cntStr xs

strToUpper :: String -> String
strToUpper [] = []
strToUpper (x:xs) = (toUpper x):(strToUpper xs)

countStrInFile :: String -> Integer
countStrInFile src = if (src==[]) then 0 else (cntStr src) +1 

addToEnd :: String -> FilePath -> IO ()
addToEnd str fname = appendFile fname ('\n':str)



addToStart :: String -> FilePath -> IO ()
addToStart str fname = do
	str1 <- readFile fname
	writeFile ("temp111"++fname) (str++['\n'])
	appendFile ("temp111"++fname) str1
	str1 <- readFile ("temp111"++fname)
	writeFile fname str1
	removeFile ("temp111"++fname)

printFileToUpper :: FilePath -> IO ()
printFileToUpper fname = do
	str1 <- readFile fname
	let str2 = strToUpper str1
	appendFile ("temp111"++fname) str2
	str1 <- readFile ("temp111"++fname)
	writeFile fname str1
	removeFile ("temp111"++fname)

concatinateFiles fname1 fname2 = undefined
--intToDigit :: Int -> Char

cccddd = chr

generateRandomFile col row = undefined

main = do
  [fname] <- getArgs
  printFileToUpper fname


