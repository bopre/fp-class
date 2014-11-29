{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.Environment

import Data.Char

type PasswordParams = (Int, Bool, Bool, Bool)

isValid :: PasswordParams -> String -> Bool
isValid (len, letr, numbr, punct) s = length s >= len && 
				((any isAlpha s)||(not letr)) && 
				((any isNumber s)||(not numbr)) && 
				((any isPunctuation s)||(not punct))

getValidPassword :: PasswordParams ->  MaybeT IO String
getValidPassword params = do
  lift $ putStrLn "Введите новый пароль:"
  s <- lift getLine
  guard (isValid params s)
  return s
 
askPassword :: PasswordParams -> MaybeT IO ()
askPassword params = do
  value <- msum $ (repeat (getValidPassword params))
  lift $ putStrLn "Сохранение в базе данных..."

main = getArgs >>= \(x:y:z:t:tail) -> (runMaybeT (askPassword ((read x), (read y), (read z), (read t))))
