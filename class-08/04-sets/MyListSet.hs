module MyListSet (MyListSet, empty, enqueue, dequeue, isEmpty) where

import MyAbstractSet

newtype MyListSet t = SetImpl [t]

instance MyAbstractSet MyListSet where
  empty = SetImpl []
  
  isEmpty (SetImpl xs) = null xs
  
  existElem (SetImpl []) _ = False
  existElem (SetImpl (x:xs)) y = ((x==y)||(existElem (SetImpl xs) y))

  addElem (SetImpl xs) x = if (existElem (SetImpl xs) x) then (SetImpl xs) else (SetImpl (x:xs))
  
  removeElem (SetImpl xs) x = (SetImpl (removeXFromList xs x))
	where
		removeXFromList (z:zs) y = if (z==y) then (removeXFromList zs y) else (z:(removeXFromList zs y))
