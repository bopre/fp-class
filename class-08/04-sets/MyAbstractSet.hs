module MyAbstractSet where

class MyAbstractSet a where
  empty :: (Eq t) => a t
  isEmpty :: (Eq t) => a t -> Bool
  addElem :: (Eq t) => a t -> t -> a t
  existElem :: (Eq t) => a t -> t -> Bool
  removeElem :: (Eq t) => a t -> t -> a t 
