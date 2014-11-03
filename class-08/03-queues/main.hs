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

queueAreSimilar :: (AbstractQueue q1, AbstractQueue q2, Num a, Eq a) => q1 a -> q2 a -> Bool
queueAreSimilar q1 q2 = if (isEmpty q1)/=(isEmpty q2) then False else if (isEmpty q1) then True else 
				let (x1,q1') = dequeue q1
				    (x2,q2') = dequeue q2
					in ((x1==x2) && (queueAreSimilar q1' q2'))

main = do
	[nS] <- getArgs
	let n = read nS
	gen <- newStdGen
	let x=createRandomList n gen
	putStrLn $ "Generated List:"
	putStrLn $ show x
	putStrLn $ "----------------"
	qNewQ <- forNdo n 0 (empty :: Q.Queue Int) x
	qNewFQ <- forNdo n 0 (empty :: FQ.Queue Int) x
	putStrLn $ show (queueAreSimilar qNewQ qNewFQ)
	--putStrLn $ show ((cntElemsInQueue qNewQ)==(cntElemsInQueue qNewFQ))


createRandomList ::  Int -> StdGen -> [Int]
createRandomList 0 _ = []
createRandomList n gen = let (x,newGen)=randomR(-100,100) gen :: (Int, StdGen)
				in x:(createRandomList (n-1) newGen)

cntElemsInQueue q = if isEmpty q then 0 else let (x,q')=dequeue q in (1+(cntElemsInQueue q'))

printQueue q = 
	if (isEmpty q) then return ()
		else do
		let (x, q') = dequeue q
		putStr $ show x ++ ", "
		printQueue q'
{-
main = do
	[nS] <- getArgs
	gen <- newStdGen
	let x=createRandomList (read nS) gen
	--putStrLn $ show x
	--putStrLn $ "----------------"
	qNew <- forNdo (read nS) 0 (empty :: Q.Queue Int) x
	printQueue qNew
-}
getIInList _ [] = error "end of bound"
getIInList 1 (x:xs) = x
getIInList n (x:xs) = getIInList (n-1) xs

lengthList [] = 0
lengthList (x:xs) = 1+(lengthList xs)

oneIterationEnq n curN q l = do
	if (n<curN) then return q
		else do
		let q' = enqueue q (getIInList n l)
		--putStrLn $ ""
		--printQueue q'
		--putStrLn $ "-----enq---------------"
		oneIterationEnq n (curN+1) q' l

oneIterationDeq n curN q l = do
	if (n<curN) then return q
		else do
		let (x, q') = dequeue q
		--putStrLn $ ""
		--printQueue q'
		--putStrLn $ "--------qec------------"
		oneIterationDeq n (curN+1) q' l


forNdo n curN q l = do
	if (n<=curN) then return q
		else do
		q' <- oneIterationEnq (curN+1) 1 q l
		q'' <- oneIterationDeq curN 1 q' l
		--putStrLn $ ""
		--printQueue q''
		--putStrLn $ "------iter--------------"
		forNdo n (curN+1) q'' l

