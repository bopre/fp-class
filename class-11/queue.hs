import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import System.Environment

type Queue = [Int]

enqueue :: Int -> State Queue ()
enqueue x = do
	xs <- get
	put (xs ++ [x])


recvHeadLast :: [a] -> (a,[a])
recvHeadLast [] = error "empty"
recvHeadLast [x] = (x,[])
recvHeadLast (x:xs) = let (y,t) = (recvHeadLast xs) in (y,x:t)


dequeue :: State Queue Int
dequeue = do
	(ll) <- get
	let (x,t) = (recvHeadLast ll)
	put t
	return x

queryTest :: State Queue Int
queryTest = do
	enqueue 5
	a <- dequeue
	dequeue
