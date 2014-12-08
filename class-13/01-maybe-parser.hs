import Control.Monad
import System.Environment
import Data.Maybe

{-
   Тип Parser может быть определён следуюшим образом:
-}

newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}

instance Monad Parser where
  return x = Parser (\s->Just (x,s))
  p >>= q = Parser (\s -> case apply p s of
				Nothing -> Nothing
				Just (x,t1) -> apply (q x) t1) 
  fail _ = Parser (\s -> Nothing)


instance MonadPlus Parser where
  mzero = Parser (\x -> Nothing)
  p `mplus` q = Parser (\s -> let ps = apply p s in if (isJust ps) then ps else apply q s)
