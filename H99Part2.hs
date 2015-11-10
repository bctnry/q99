-- Problem 31 to 41
module H99Part2 where
import Control.Applicative -- used in Problem 40

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x = and $ map (\y -> (x `mod` y) /= 0)[2..(gtX x 1)]
          where gtX x y = if (y*y) > x then y else gtX x (y+1)

myGCD :: Int -> Int -> Int
myGCD x y = if x == 0 then y else (myGCD (y `mod` x) x)

coprime :: Int -> Int -> Bool
coprime x y = if (myGCD x y) == 1 then True else False

totient :: Int -> Int
totient 1 = 1
totient x = length $ filter (coprime $ x) [1..x]

primesTo :: Int -> [Int]
primesTo x = filter isPrime [1..x]

primeFactors :: Int -> [Int]
primeFactors x = primeFactorsInt x (primesTo (x `div` 2)) []
               where primeFactorsInt x [] res = res
                     primeFactorsInt x (y:ys) res = if (mod x y) == 0 
                                                  then primeFactorsInt (div x y) (y:ys) 

(res++[y])
                                                  else primeFactorsInt x ys res

-- Taken from H99Part1.hs
pack' :: (Eq a) => [a] -> [[a]]
pack' [] = [[]]
pack' [x] = [[x]]
pack' l@(x:y:xs) = takeWhile (/=[]) ((takeWhile (==x) l):(pack' (dropWhile (==x) l)))


encode' :: (Eq a) => [a] -> [(a,Int)]
encode' [] = []
encode' x = map rewrite (pack' x)
         where rewrite x = ((head x),(length x))

primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult = encode' . primeFactors

primesR :: Int -> Int -> [Int]
primesR x y = filter (>x) (primesTo y)

goldbach :: Int -> [(Int, Int)]
goldbach x = filter (/=(0,0)) (map (\z -> retrFunc z x) list)
           where primeList = primesTo x
                 tripleOf x y = (x,y,(x+y))
                 firstTwo (x,y,_) = (x,y)
                 list = tripleOf <$> primeList <*> primeList
                 retrFunc (x,y,z) a = if z == a then (x,y) else (0,0)

goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList x y = map (head . goldbach) (filter (\x -> (mod x 2) == 0) [x..y])
