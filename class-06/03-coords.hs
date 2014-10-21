{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}

import System.Random
import System.Environment
import Data.List

data Point = Point Double Double
	deriving(Eq, Show, Read)

gen_point :: StdGen -> (Point, StdGen)
gen_point gen =
	let (x, newGen) = randomR(-500,500) gen :: (Double, StdGen)
	    (y, newGen') = randomR(-500,500) newGen :: (Double, StdGen)
	    in (Point x y, newGen')
get_points :: Int -> StdGen -> [Point]
get_points 0 _ = []
get_points n gen =
	let (p, newGen) = gen_point gen in
	(p:(get_points (n-1) newGen))

createRandomPointsFile :: FilePath -> IO ()
createRandomPointsFile fname = do
	gen <- newStdGen
	let (nlines, newGen) = randomR(0,100) gen :: (Int, StdGen)
	let pts = get_points nlines newGen
	let pts_lines = map show pts
	writeFile fname (unlines pts_lines)

read_point :: [String] -> Point
read_point ["Point", x, y] = Point (read x) (read y)

get_points_from_text :: String -> [Point]
get_points_from_text textstr = map (read_point . words) $ lines textstr
points_from_file fname = do
	contents <- readFile fname
	return $ get_points_from_text contents
point_in_quarter 1 (Point x y) = (x >= 0) && (y >= 0)
point_in_quarter 2 (Point x y) = (x < 0) && (y >= 0)
point_in_quarter 3 (Point x y) = (x < 0) && (y < 0)
point_in_quarter 4 (Point x y) = (x >= 0) && (y < 0)
points_in_quarter q iopts = do
	putStr "In quater "
	putStr (show q)
	putStr " точек: "
	pts <- iopts
	putStrLn (show $ length $ filter (point_in_quarter q) pts)

dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) = sqrt((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1))
compare_point :: Point -> Point -> Ordering
compare_point a b =
	let dist_a = dist a (Point 0 0)
	    dist_b = dist b (Point 0 0) in
		if (dist_a > dist_b) then GT
		else if (dist_a < dist_b) then LT
			else EQ

recvPointsQuaters :: IO([Point]) -> IO()
recvPointsQuaters pts = do
	points_in_quarter 1 pts
	points_in_quarter 2 pts
	points_in_quarter 3 pts
	points_in_quarter 4 pts

max_dist_point :: [Point] -> Point
max_dist_point = maximumBy compare_point

recvTheMostFarPoint :: IO([Point]) -> IO()
recvTheMostFarPoint iopts = do
	pts <- iopts
	putStr "The most far point: "
	putStrLn $ show $ max_dist_point pts

main = do
	args <- getArgs
	myCase args
myCase ["create",fname] = createRandomPointsFile fname
myCase ["quaters",fname] = recvPointsQuaters $ points_from_file fname
myCase ["far",fname] = recvTheMostFarPoint $ points_from_file fname
myCase _ = do
	putStrLn "Info"
	putStrLn " \"create\" fname - gen new random file"
	putStrLn " \"quaters\" fname - get quaters"
	putStrLn " \"far\" fname - get the mos far point"
