import Parser
import SimpleParsers
import ParseNumbers
import System.Environment
import Control.Applicative

{- Напишите парсер для вещественных чисел. -}

float :: Parser Float
float = (*) <$> minus <*> positive_float
	where
		minus = (char '-' >> return (-1)) <|> return 1

digitF :: Parser Float
digitF = digitToFloat `fmap` sat isDigit

digitToFloat :: Char -> Float
digitToFloat c
	|c=='0'=0
	|c=='1'=1
	|c=='2'=2
	|c=='3'=3
	|c=='4'=4
	|c=='5'=5
	|c=='6'=6
	|c=='7'=7
	|c=='8'=8
	|c=='9'=9

isDigit :: Char -> Bool
isDigit c = (c>='0')&&(c<='9')


positive_float :: Parser Float
positive_float = ((+) <$> natural2 <*> pointNatural) <|> natural2
	where pointNatural = (\x y-> if (x==0) then 0 else  (y / x) ) <$> ppoint <*> step1
	      ppoint = (char '.' >> return 10)
	      step1 = foldr1 (\m n -> (m / 10) + n) `fmap` many12 digitF
	      natural2 = foldl1 (\m n -> m*10 + n) `fmap` many12 digitF

{-
	where 	f [] = []
		f str = let (r1,t1,has_comma) = findComma str in if (r1 == "") then [] 
							else if has_comma then let r2 = (findTail t1) in if (r2/="") then read(r1++('.':r2)) else (read r1)
						     else (read r1)
	        findComma [] = ([],[],True)
	        findComma (x:xs) = if (x>'0')&&(x<'9') then 
							let (r,t, hc)=findComma xs 
								in (x:r,xs,hc) else if (x=='.') then ([],xs,True) else ([],xs,False)
	        findTail [] = []
	        findTail (x:xs) = if (x>'0')&&(x<'9') then x:(findTail xs) else []
-}

natural1 = foldl1 (\m n -> m*10 + n) `fmap` many12 digit

many2 :: Parser a -> Parser [a]
many2 p = many12 p <|> return []

many12 :: Parser a -> Parser [a]
many12 p = (:) <$> p <*> many2 p

{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}
complex :: Parser (Float, Float)
complex = undefined

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = undefined

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = undefined

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = undefined


