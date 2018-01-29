-- Exercise 1

sumSq :: Int
sumSq = sum [x^2 | x <- [1..100]]

-- Exercise 2

grid :: Int -> Int -> [(Int,Int)]
grid x y = [(i,j) | i <- [0..x], j <- [0..y]]

-- Exercise 3

square :: Int -> [(Int,Int)]
square x = [tup | tup <- grid x x, uncurry (/=) tup]
