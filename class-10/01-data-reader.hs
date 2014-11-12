{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}
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

read1 str = let (n:a:g:tail)=(words str) in (Person n (read a) g)

{-
data Complex a = Complex a a
instance (Show a) => Show (Complex a) where
	show (Complex re im) = show re ++ " + i*"++show im
-}

personName :: Person -> String
personName p = name p
main = undefined


