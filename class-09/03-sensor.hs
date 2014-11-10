import System.Environment
import Data.Monoid
import Data.Maybe

{-
  Некоторый датчик генерирует по пять сигналов в сутки, часть из которых
  не доходит до базовой станции. Полученные от датчика сведения представлены
  текстовым файлом, содержащим по одному целому числу в каждом строке. Если
  сигнал не был получен, вместо числа в файле записывается прочерк (символ '-').
-}

type SensorValue = Maybe Int
type SensorData = [SensorValue]

{- Напишите функцию, которая преобразует прочитанную из файла строку в список
   значений, полученных от датчика. -}

getData :: String -> SensorData
getData s = map (\line -> let (x:xs) = (words line) in if (x=="-") then Nothing else Just (read x)) (lines s)

{- Напишите функцию, группирующую данные по суткам. -}

dataByDay :: SensorData -> [SensorData]
dataByDay [] = []
dataByDay (x1:x2:x3:x4:x5:xs) = [x1,x2,x3,x4,x5]:(dataByDay xs)

{-
  Посчитайте минимальное значение среди показаний датчика,
  полученных:
  а) первыми в течение суток;
  б) последними в течение суток.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов First и Last,
  при этом должна быть написана одна функция, отвечающая на вопрос а) или б)
  в зависимости от значения логического параметра.
-}

minData1 :: Bool -> [SensorData] -> Int
minData1 needFirst t = minimum $ map (firstOrLast) $ filter (any (\x->x/=Nothing)) t
	where  firstOrLast l = if needFirst then fromJust. getFirst . mconcat . map First $ l else fromJust. getLast . mconcat . map Last $ l

{-
  Посчитайте минимальное значение среди данных,
  полученных:
  а) как суммы всех показаний датчика за каждые сутки;
  б) как произведения всех показаний датчика за каждые сутки.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов Sum, Product
  и Maybe a, где a — моноид, при этом должна быть написана одна функция, отвечающая
  на вопрос а) или б) в зависимости от значения логического параметра.
-}

minData2 :: Bool -> [SensorData] -> Int
minData2 needSum t = minimum $ map (sumOrProduct) $ filter (any (\x->x/=Nothing)) t
	where  sumOrProduct l = if needSum then getSum . mconcat . map Sum $ map (fromMaybe 0) l else getProduct . mconcat . map Product $ map (fromMaybe 1) l

{- Попробуйте объединить две предыдущие функции в одну. -}

data SensorTask = NeedFirst | NeedLast | NeedSum | NeedProduct

minData :: SensorTask -> [SensorData] -> Int
minData NeedFirst = minData1 True
minData NeedLast = minData1 False
minData NeedSum = minData2 True
minData NeedProduct = minData2 False

{-
  Пользуясь моноидами All, Any и любыми другими, выясните следующую информацию:
  1) количество суток, за которые не было получено ни одного показания;
  2) количество суток, показания за которые получены полностью;
  3) количество суток, за которые было получено хотя бы одно показание;
  4) количество суток, сумма показаний за которые превосходит заданное число;
  5) количество суток, произведение показаний за которые превосходит заданное число;
  6) количество суток, первое показание за которые превосходит заданное число;
  7) количество суток, последнее показание за которые превосходит заданное число.

  Постарайтесь ответить на все вопросы, написав одну функцию.
-}
data SensorTask2 = WithoutAnything | AllReading | AnyReading | SumIsBiggerThan | ProductionIsBiggerThan | FirstIsBigger | LastIsBigger


countDays :: Int -> SensorTask2 -> [SensorData] -> Int
countDays br sn2 t = countTrue $ map (someAction sn2) t
	where  someAction WithoutAnything l= getAll . mconcat . map All $ map (\x->x==Nothing) l
	       someAction AllReading l= getAll . mconcat . map All $ map (\x->x/=Nothing) l
	       someAction AnyReading l=getAny . mconcat . map Any $ map (\x->x/=Nothing) l
      	       someAction SumIsBiggerThan l=(getSum . mconcat . map Sum $ map (fromMaybe 0) l)>br
	       someAction ProductionIsBiggerThan l=(getProduct . mconcat . map Product $ map (fromMaybe 1) l)>br
	       someAction FirstIsBigger l=if (not (someAction WithoutAnything l)) then ((fromJust. getFirst . mconcat . map First $ l)>br) else False
	       someAction LastIsBigger l=if (not (someAction WithoutAnything l)) then ((fromJust. getLast . mconcat . map Last $ l)>br) else False
	       countTrue [] = 0
	       countTrue (x:xs) = if x then 1+(countTrue xs) else (countTrue xs)


main = do
  [fname,xStr] <- getArgs
  let x = read xStr :: Int
  sData <- getData `fmap` readFile fname
  let dataByDays = dataByDay sData
  putStrLn $ "Days without Data" ++ show (countDays x WithoutAnything dataByDays)
  putStrLn $ "Days with all Data" ++ show (countDays x AllReading dataByDays)
  putStrLn $ "Days with any Data" ++ show (countDays x AnyReading dataByDays)
  putStrLn $ "Days with Data sum bigger than " ++ show x ++ " " ++ show (countDays x SumIsBiggerThan dataByDays)
  putStrLn $ "Days with Data production bigger than " ++ show x ++ " " ++ show (countDays x ProductionIsBiggerThan dataByDays)
  putStrLn $ "Days with first Data bigger than " ++ show x ++ " " ++ show (countDays x FirstIsBigger dataByDays)
  putStrLn $ "Days with last Data bigger than " ++ show x ++ " " ++ show (countDays x LastIsBigger dataByDays)
  return ()
