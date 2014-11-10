import Control.Applicative
import System.Environment

{-
  Пользуясь возможностями аппликативных функторов, определите функцию, 
  вычисляющую наибольший из результатов двух вычислений (значений в некотором
  контексте), принимаемых в качестве параметров (для результатов имеется
  экземпляр класса типов Ord).
-}

maxApp2 :: (Ord a, Applicative f) => f a -> f a -> f a
maxApp2 a1 a2 = (\x y -> if (x>y) then x else y) <$> a1 <*> a2

{- Реализуйте аналогичную функцию в случае трёх заданных значений в контексте. -}

maxApp3 :: (Ord a, Applicative f) => f a -> f a -> f a -> f a
maxApp3 a1 a2 a3 = maxApp2 a1 (maxApp2 a2 a3)

{-
maxApp3 :: (Ord a, Applicative f) => f a -> f a -> f a -> f a
maxApp3 a1 a2 a3 = (\x y -> if (x>y) then x else y) <$> ((\x y -> if (x>y) then x else y) <$> a1 <*> a2) <*> a3
-}
{- Реализуйте аналогичную функцию в случае списка значений в контексте. -}

 :: (Ord a, Applicative f) => [f a] -> f a
maxApp [x] = x
maxApp (x:xs) = max <$> x <*> maxApp xs

{-
  Продемонстрируйте использование написанных функций для аппликативных функторов Maybe,
  список (для каждого из двух экземпляров), Either String и IO.
-}


main = do
	x <- maxApp3 (getLine) (getLine) (getLine)
	putStr "Max is "
	putStrLn $ show x

	return ()

{- (необязательно)
  Ясно ли вам, что вы реализовали нечто, похожее на моноид на аппликативных функторах?
  Можете ли вы выразить это в коде? Необходимо ли добавлять какие-нибудь ограничения?
-}
