{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}
import System.Random
import System.Environment

createRandomStr :: Int -> Int -> Int -> StdGen -> (String, StdGen)
createRandomStr _ _ 0 gen = ("",gen)
createRandomStr minEl maxEl 1 gen = let (x, newGen) = randomR(minEl,maxEl) gen :: (Int, StdGen)
					    in ((show x), newGen)
createRandomStr minEl maxEl n gen = let (x, newGen) = randomR(minEl,maxEl) gen :: (Int, StdGen)
				        (tale, newGen') = (createRandomStr minEl maxEl (n-1) newGen)
					 in ((((show x)++", ")++tale), newGen')


createRandomText :: Int -> Int -> Int -> Int -> StdGen -> [String]
createRandomText _ _ _ 0 gen = []
createRandomText minEl maxEl col 1 gen = let (str, newGen) = createRandomStr minEl maxEl col gen
					     in [str]
createRandomText minEl maxEl col n gen = let (str, newGen) = createRandomStr minEl maxEl col gen
					     in str:(createRandomText minEl maxEl col (n-1) newGen)

createRandomFile :: String -> Int -> Int -> Int -> Int -> IO ()
createRandomFile fname minElem maxElem col row = do
	gen <- newStdGen
	let int_lines = createRandomText minElem maxElem col row gen
	writeFile fname (unlines int_lines)



main = do
	[fname, minElem, maxElem, col, row] <- getArgs
	createRandomFile fname (read minElem) (read maxElem) (read col) (read row)

