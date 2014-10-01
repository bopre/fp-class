{-
Явная рекурсия в решениях хотя и допускается, но не приветствуется. Старайтесь обходиться стандартными
функциями, используя при этом создание функций «на лету». Пытайтесь максимально упростить уже написанные
решения, применяя подходящие функции из модуля Data.List и любых других модулей. Перед выполнением заданий
изучите примеры из лекции по функциям высшего порядка.
-}
{-
1. Простейшие задачи на применение функций map и filter.
1.1 Преобразовать данный список целых чисел следующим образом:
a) увеличить все его элементы в два раза;-}
doubleList = map (*2)

--b) увеличить все его элементы с четными значениями в два раза;
{-
doubleIfEven a
	|a `mod`2==0=a*2
	|otherwise=a
-}	

doubleListOnlyEven = map (doubleIfEven)
	where
		doubleIfEven a
			|a `mod`2==0=a*2
			|otherwise=a

--с) обнулить все его элементы с нечетными значениями;
nullListOnlyOdd = map (nullIfOdd)
	where
		nullIfOdd a
			|a `mod`2==0=a
			|otherwise=0
--d) удалить из него элементы, большие заданного числа k;
filterLowerK k = filter (filtK)
	where
		filtK a
			|a<=k=True
			|otherwise=False
--e) отфильтровать его, оставив в списке только отрицательные числа;
filterNegatives = (filterLowerK (-1))
--f) удалить из него все положительные чётные числа.

deletePositiveEven = filter (rule')
	where
		rule' a
			|(a>0)&&(a `mod` 2 ==0)=False
			|otherwise=True


f11a :: Integral a => [a] -> [a]
f11a = map undefined
{-
1.2 Дан список декартовых координат точек на плоскости (пар вещественных чисел).
Преобразовать его следующим образом:
import Char
a) отфильтровать список так, чтобы в нём остались точки из заданной координатной четверти;
-}
type Point = (Double, Double)
type PointPolar = (Double, Double)

isInFirst :: Point -> Bool
isInFirst (a,b)
	|(a>0)&&(b>0)=True
	|otherwise=False
isInSecond (a,b)
	|(a<0)&&(b>0)=True
	|otherwise=False
isInThird :: Point -> Bool
isInThird (a,b)
	|(a<0)&&(b<0)=True
	|otherwise=False
isInForth :: Point -> Bool
isInForth (a,b)
	|(a>0)&&(b<0)=True
	|otherwise=False
isOnLines :: Point -> Bool
isOnLines (a,b)
	|(a==0)||(b==0)=True
	|otherwise=False
filterByCoordQuarter q
	|q==1=filter (isInFirst)
	|q==2=filter (isInSecond)
	|q==3=filter (isInThird)
	|q==4=filter (isInForth)
	|otherwise= error("Wrong quarter")
sqr' a = a*a
--b) преобразовать декартовы координаты в полярные.
decartToPolarCoordList = map (decartToPolarCoord)
	where decartToPolarCoord (x,y)=if ((r x y)/=0) then ((r x y),acos (x/(r x y))) else (0,acos 0)
	      r x y = sqrt ((sqr' x) + (sqr' y))

{-
1.3 Дан список слов.
a) Преобразовать все слова к верхнему регистру.
-}
toUpper' a
	|a=='q'='Q'
	|a=='w'='W'
	|a=='e'='E'
	|a=='r'='R'
	|a=='t'='T'
	|a=='y'='Y'
	|a=='u'='U'
	|a=='i'='I'
	|a=='o'='O'
	|a=='p'='P'
	|a=='a'='A'
	|a=='s'='S'
	|a=='d'='D'
	|a=='f'='F'
	|a=='g'='G'
	|a=='h'='H'
	|a=='j'='J'
	|a=='k'='K'
	|a=='l'='L'
	|a=='z'='Z'
	|a=='x'='X'
	|a=='c'='C'
	|a=='v'='V'
	|a=='b'='B'
	|a=='n'='N'
	|a=='m'='M'
	|otherwise = a

listToUpperRegister = map (toUpperReg)
	where toUpperReg str = map toUpper' str
--b) Извлечь из него подсписок слов заданной длины.
listLength' [] = 0
listLength' (x:xs) = 1+(listLength' xs)

takeByLength l = filter (lengthIs)
	where lengthIs s = (listLength' s)==l
--c) Извлечь из него подсписок слов, начинающихся с заданной буквы.
takeByFirstSymbol a = filter (firstIsA)
	where firstIsA [] = False
	      firstIsA (x:xs) = x==a


f13a :: [String] -> [String]
f13a = map undefined
{-
2. Формирование числовых последовательностей (iterate).
a) Список натуральных чисел, начиная с 0.
-}
getNaturalNumbers = iterate (1+) 0
--b) Список чётных чисел.
getEvenNumbers = iterate (2+) 0
--c) Список элементов последовательности: a0=1, an=(1+an-1)/2.
getSequnce2C = iterate (\x->(1+x)/2) 1
--d) Список символов английского алфавита.
getAlphabet = ['a'..'z']
--e) Список строк, представляющих n-значные двоичные числа.
insertToLast [] x = [x]
insertToLast (y:ys) x = y:(insertToLast ys x)

invertList [] = []
invertList (x:xs) = insertToLast (invertList xs) x

increaseStr s = step $ invertList s
	where step [] = [] 
	      step ('0':xs) = invertList ('1':xs)
	      step ('1':xs)= invertList ('0':(increaseFurther xs))
	      increaseFurther [] = []
	      increaseFurther ('0':xs)='1':xs
	      increaseFurther ('1':xs)='0':(increaseFurther xs)	
twoRaisingN 0 = 1
twoRaisingN n = 2 * (twoRaisingN (n-1))
getNDigitBinaryNumber n = take (twoRaisingN n) (iterate (increaseStr) (take (n) (iterate (\x->'0') '1' )))


nats :: [Integer]
nats = iterate undefined 0
{-
3. Группировка списков.-}
--a) Дан список символов. Сгруппировать подряд идущие символы по принципу: цифры — не цифры — ...

--b) Дан список пар вещественных чисел (координат точек на плоскости). Сгруппировать подряд идущие
--координаты точек, лежащие в одной координатной четверти.

--c) Дан список и ненулевое натуральное число n. Разбить список на подсписки длиной n каждый.
--Последний подсписок может содержать менее n элементов.

--d) Дан список и ненулевые натуральные числа n и m. Разбить список на перекрывающиеся подсписки
--длиной n элементов со сдвигом относительно предыдущего подсписка на m элементов.

--e) Дан список. Определить длину самого длинного подсписка, содержащего подряд идущие одинаковые элементы.

f3d :: [a] -> Int -> Int -> [[a]]
f3d xs n m = undefined
-- Должно быть True
test_f3d = f3d [1..10] 4 2 == [[1,2,3,4],[3,4,5,6],[5,6,7,8],[7,8,9,10],[9,10]]

isDigit' a
	|(a=='0')||(a=='1')||(a=='2')||(a=='3')||(a=='4')||(a=='5')||(a=='6')||(a=='7')||(a=='8')||(a=='9')=True
	|otherwise=False
{-
4. Разные задачи.
a) Дан текст в виде строки символов, содержащий среди прочего числовые данные. Посчитать количество
всех упоминающихся в тексте чисел.
-}
countNumbers [] = 0
countNumbers (x:xs)
	|(isDigit' x)=1+(countNumbers (passDigit xs))
	|otherwise=countNumbers xs
	where passDigit []=[]
	      passDigit (x:xs)
			|(isDigit' x)=passDigit xs
			|otherwise=xs
--b) Найти сумму всех чисел Фибоначчи, удовлетворяющих заданному предикату, в указанном промежутке
--(например: все чётные от 1 до 106).
--sumByPredicate f l = map filter f l
--c) Дана строка текста и число n. Сформировать список, содержащий n самых часто используемых
--в строке символов.

--d) Дан список чисел. Сформировать список локальных максимумов исходного списка. Локальным максимумом
--называется элемент, больший своих соседей.

--e) Дан список. Продублировать все его элементы.
