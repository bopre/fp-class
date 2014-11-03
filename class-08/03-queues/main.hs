import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ
import System.Random
import System.Environment

checkQueue :: (AbstractQueue q, Num a, Eq a) => q a -> Bool
checkQueue q = lastElem (enqueue q 5) == 5
 where
  lastElem q = let (x, q') = dequeue q in
               if isEmpty q' then x else lastElem q'
{-
main = print $
         checkQueue (enqueue empty 10 :: Q.Queue Int)
         &&  checkQueue (enqueue empty 10 :: FQ.Queue Int)
-}

createRandomList ::  Int -> StdGen -> [Int]
createRandomList 0 _ = []
createRandomList n gen = let (x,newGen)=randomR(-100,100) gen :: (Int, StdGen)
				in x:(createRandomList (n-1) newGen)

printQueue q = 
	if (isEmpty q) then return ()
		else do
		let (x, q') = dequeue q
		putStr $ show x ++ ", "
		printQueue q'

main = do
	[nS] <- getArgs
	gen <- newStdGen
	let x=createRandomList (read nS) gen
	putStrLn $ show x
	putStrLn $ "----------------"
	qNew <- forNdo (read nS) 0 (empty :: Q.Queue Int) x
	printQueue qNew

getIInList _ [] = error "end of bound"
getIInList 1 (x:xs) = x
getIInList n (x:xs) = getIInList (n-1) xs

lengthList [] = 0
lengthList (x:xs) = 1+(lengthList xs)

oneIterationEnq n curN q l = do
	if (n<curN) then return q
		else do
		let q' = enqueue q (getIInList n l)
		putStrLn $ ""
		printQueue q'
		putStrLn $ "-----enq---------------"
		oneIterationEnq n (curN+1) q' l

oneIterationDeq n curN q l = do
	if (n<curN) then return q
		else do
		let (x, q') = dequeue q
		putStrLn $ ""
		printQueue q'
		putStrLn $ "--------qec------------"
		oneIterationDeq n (curN+1) q' l


forNdo n curN q l = do
	if (n<=curN) then return q
		else do
		q' <- oneIterationEnq (curN+1) 1 q l
		q'' <- oneIterationDeq curN 1 q' l
		putStrLn $ ""
		printQueue q''
		putStrLn $ "------iter--------------"
		forNdo n (curN+1) q'' l

