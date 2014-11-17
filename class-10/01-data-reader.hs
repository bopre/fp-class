{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}
import Control.Monad 
import System.Environment

data Person = Person {name::String, age::Int, group::String}
	deriving (Eq)


instance Ord Person where
	compare p1 p2
		|(name p1 /= name p2) = if ((name p1) > (name p2)) then GT else LT
		|(group p1 /= group p2) = if ((group p1) > (group p2)) then GT else LT
		|(age p1 /= age p2) = if ((age p1) > (age p2)) then GT else LT
		|otherwise =EQ 
{-	
	(>) p1 p2
		|(name p1 /= name p2) = ((name p1) > (name p2))
		|(group p1 /= group p2) = ((group p1) > (group p2))
		|otherwise = ((age p1) > (age p2))
	(==) p1 p2 = (name p1) == (name p2)&&((age p1) == (age p2)&&(group p1) == (group p2))
-}
instance Show Person where
	show p = "Name: `"++name p++"`, "++"Age: `"++show (age p)++"`, "++"Group: `"++group p++"`"

readPersons :: [String] -> [Person]
readPersons [] = []
readPersons (x:xs) = (readPerson x):(readPersons xs)

readPerson :: String -> Person
readPerson str = let (n:a:g:tail)=(words str) in (Person n (read a) g)

{-
data Complex a = Complex a a
instance (Show a) => Show (Complex a) where
	show (Complex re im) = show re ++ " + i*"++show im
-}

concatinate_list l1 [] = l1
concatinate_list [] l2 = l2
concatinate_list (x:xs) l2 = x:(concatinate_list xs l2)

sort_list :: (Ord a) => [a] -> [a]
sort_list [] = []
sort_list [x] = [x]
sort_list [x,y] = if (x>y) then [x,y] else [y,x]
sort_list (x:xs) = concatinate_list (sort_list (filter (\y->y<x) xs)) (x:(sort_list ((filter (\y->y>=x) xs))))

personName :: Person -> String
personName p = name p

personsToStr :: [Person] -> String
personsToStr [] = ""
personsToStr (x:xs) = (show x) ++ "\n" ++ (personsToStr xs)

getFirst (x:y:z:xs) = x
getSecond (x:y:z:xs) = y
getThird (x:y:z:xs) = z

personsFromFile :: FilePath -> IO [Person]
personsFromFile fname = readFile fname >>= (return . (map (\x -> readPerson x) ) . lines)


main = (++) `liftM` personsFromFile "1.txt" `ap` personsFromFile "2.txt" >>= writeFile "3.txt" . personsToStr . sort_list
