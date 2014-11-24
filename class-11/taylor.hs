import Control.Monad
import Control.Monad.Writer
import System.Environment


--left is cos; right is sin
sincosTaylor :: Double -> Writer [(Double, Double)] (Double,Double)
sincosTaylor a = iteration 1 1 1 1 a 0.001 0 0 --  tell [(a,0)] >> return a
	where
		iteration :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Writer [(Double, Double)] (Double,Double)
		iteration curSign iterNum curDeg curFact x eps sum_cos_i sum_sin_i = let cos_i = ((curSign * curDeg) / curFact) :: Double
											 sin_i = ((curSign * (curDeg*x)) / (curFact*iterNum)) :: Double
												in tell [(cos_i, sin_i)] >> (if ((abs cos_i)<eps)&&((abs sin_i)<eps) then (return (sum_cos_i+cos_i, sum_sin_i + sin_i)) else (iteration (curSign * (-1)) (iterNum+1) ((curDeg*x)*x) ((curFact*iterNum)*(iterNum+1)) x eps (sum_cos_i+cos_i) (sum_sin_i + sin_i)))
	

main = getArgs >>= return . runWriter . sincosTaylor . read . head
