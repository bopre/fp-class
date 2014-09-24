-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms s = (h, m, s `mod` 60)
	where 
		h = s `div` 3600
		m = ((s `mod` 3600) `div` 60)
hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = s + (m*60) + (h*3600)
-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h,m,s)
-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]
-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.
type Point = (Double, Double)

sqr' :: Double -> Double
sqr' a = a*a

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt (sqr' (x1-x2) + sqr' (y1-y2))

sum3 :: Double -> Double -> Double -> Double
sum3 a b c = a+(b+c)

triangle :: Point -> Point -> Point -> (Double, Double)
triangle p1 p2 p3 = (p, s)
	where
		p' = p/2
		a1 = distance p1 p2
		b1 = distance p2 p3
		c1 = distance p3 p1
		p = sum3 a1 b1 c1
		s = sqrt $ p' * ((p'-a1) * ((p'-b1) * (p'-c1)))
-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.
-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs)
	|x `mod` 2 == 0 = 1 + (nEven(xs))
	|x `mod` 2 /= 0 = nEven(xs)
-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
-- [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs)= (2*x) : (doubleElems xs)
-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs)
	|x `mod` 2 == 0 = fltOdd xs
	|x `mod` 2 /= 0 = x: (fltOdd xs)
-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
delAllNegative :: (Ord a, Num a) => [a] -> [a]
delAllNegative [] = []
delAllNegative (x:xs)
	|x<0 = delAllNegative xs
	|otherwise = x : (delAllNegative xs)

-- б) увеличить элементы с чётными значениями в два раза;
doubleEvenElems :: Integral a => [a] -> [a]
doubleEvenElems [] = []
doubleEvenElems (x:xs)
	|x `mod` 2 == 0 = (2*x): (doubleEvenElems xs)
	|otherwise = x: (doubleEvenElems xs)
-- в) переставить местами чётные и нечётные по порядку следования элементы
-- (для списков нечётной длины отбрасывать последний элемент).

inverseOddEvenPosition [] = []
inverseOddEvenPosition [a] = []
inverseOddEvenPosition (x:(y:xs)) = y:(x:(inverseOddEvenPosition xs))

-- 2.5
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = (x+y) : (combine_plus xs ys)
-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
recievePairs :: [Integer] -> [Integer] -> [(Integer, Integer)]
recievePairs [] _ = []
recievePairs _ [] = []
recievePairs (x:xs) (y:ys)
	|p==[] = [(x,y)]
	|otherwise=(x,y):p
		where p = recievePairs xs ys
-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
seqNdec :: Integer -> [Integer]
seqNdec n
	|n==0=[]
	|n<1 = error "Wrong N"
	|otherwise = n:(seqNdec (n-1))


-- б) в порядке возрастания.
seqNinc :: Integer -> [Integer]
seqNinc n 
	|n<1=error "Wrong N"
	|otherwise= step 1
	where step i
		|i==n=[n]
		|otherwise=i:(step $ i+1)
-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.

insertBorder_a _ [] = []
insertBorder_a _ [x] = [x]
insertBorder_a a (x:xs)=x:(a:(insertBorder_a a xs))


deleteFromList _ []=[]
deleteFromList a (x:xs)
	|a==x = deleteFromList a xs
	|otherwise = x:(deleteFromList a xs)

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).


separateListByFirstElem [] = ([], [])
separateListByFirstElem [x] = ([x],[])
separateListByFirstElem (x:xs) = ((x:(step1 xs)),(step2 xs))
	where 
		step1 []=[]
		step1 (y:ys)
			|y==x=x:(step1 ys)
			|otherwise=[]
		step2 []=[]
		step2 (y:ys)
			|y==x=(step2 ys)
			|otherwise=(y:ys)




--separateListByFirstElem [] = ([], [])
--separateListByFirstElem [x] = ([x],[])
--separateListByFirstElem (x:xs) = ((x:(step xs)),(deleteFromList x xs))
--	where 
--		step []=[]
--		step (y:ys)
--			|y==x=x:(step ys)
--			|otherwise=step ys

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
--выдать элемент списка по номеру
recv_i (x:xs) i
	|i<0=error "Wrong index"
	|(xs==[])&&(i>1)=error "Wrong index"
	|i==0 = x
	|i>0= recv_i xs (i-1)

-- б) Eq a => [a] -> a -> Bool
--наличие элемента в списке
list_contains [] a = False
list_contains (x:xs) a
	|x==a=True
	|otherwise=list_contains xs a
-- в) [a] -> Int -> [a]

-- г) a -> Int -> [a]
--построить список из одного элемента, размноженного указанное количество раз
build_list a 0 = []
build_list a n = a:(build_list a (n-1))


add_to_the_end [] a = [a]
add_to_the_end (x:xs) a = x:(add_to_the_end xs a)

invert_list [] = []
invert_list (x:xs) = add_to_the_end (invert_list xs) x
-- д) [a] -> [a] -> [a]
--конкатенация двух списков
concatinate_list [] [] = []
concatinate_list [] a = a
concatinate_list a [] = a
concatinate_list a (y:ys) = concatinate_list (add_to_the_end a y) ys
-- е) Eq a => [a] -> [[a]]


-- ж) [a] -> [(Int, a)]
--пары элементов: элемент-индекс
make_with_index [] = []
make_with_index l = step l 0
	where
		step [x] i = [(x,i)]
		step (x:xs) i = (x,i):(step xs (i+1))
-- з) Eq a => [a] -> [a]
