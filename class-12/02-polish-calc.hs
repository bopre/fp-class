{-
   Представленный на лекции вычислитель выражений в обратной польской нотации
   не проверяет корректность заданного выражения, выдавая, к примеру, следующие
   очевидно неправильные ответы:

   ghci> evalRPN "* 1"
   1
   ghci> evalRPN "+ * 2 4"
   4
   ghci> evalRPN "* * *"
   *** Exception: 01-polish-calc.hs:10:15-43: Non-exhaustive patterns in lambda

   1. Переработайте реализацию вычислителя таким образом, чтобы в случае ошибки ответ
   не выводился. Воспользуйтесь в решении монадой Maybe, совместив её с монадой State
   с помощью преобразователя монад.

   2. Добавьте к вычислителю поддержку переменных. Значения переменных должны
   задаваться в командной строке, а доступ к ним должен осуществляться средствами
   монады Reader.

   3. Добавьте к вычислителю подсчёт количества операций со стеком (монада Writer
   с журналом типа Sum Int).
-}


import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe

type Stack = [Int]

push :: Int -> State Stack ()
push x = get >>= put . (x:)

isOper :: String -> Bool
isOper x = (x=="*")||(x=="/")||(x=="+")||(x=="-")

isNumber :: String -> Bool
isNumber x = all (\z->(z>='0')&&(z<='9')) x

cntOp :: String -> (Int,String)
cntOp str = step (words str)
	where step [] = (0,"")
	      step (x:xs) = if(isOper x) then let (y,t)=(step xs) in ((1+y),t) else (0,(foldl (\x1 t1 -> x1 ++ " " ++ t1) x xs))

cntNumbers :: String -> (Int, Bool)
cntNumbers str = step (words str)
	where step [] = (0,True)
	      step (x:xs)= if(isNumber x) then let (y,b)=(step xs) in ((1+y),b) else (0,False)

isGoodRecord :: String -> Maybe String
isGoodRecord str = let (x,t) = (cntOp str) in let (y,b) = (cntNumbers t) in if (b)&&((x+1)==y) then (Just str) else Nothing

pop :: State Stack Int
pop = get >>= \(x:xs) -> put xs >> return x

evalRPN :: String -> Maybe Int
evalRPN xs = case (isGoodRecord xs) of
					Nothing -> Nothing
					Just _ -> Just (head $ execState (mapM step $ words xs) [])
  where
    step "+" = processTops (+)
    step "*" = processTops (*)
    step  n  = push (read n)
    processTops op = op `liftM` pop `ap` pop >>= push
