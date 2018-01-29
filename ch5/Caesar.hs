import Data.Char

lowers  ::  String  ->  Int
lowers  xs  = length  [x  | x <-  xs, isAsciiLower x]

count ::  Char  ->  String  ->  Int
count x xs  = length  [x' | x'  <-  xs, x ==  x']

let2int :: Char -> Int
let2int c = ord c - ord 'a'

positions ::  Eq  a =>  a ->  [a] ->  [Int]
positions x xs  = [i  | (x',i)  <-  zip xs  [0..],  x ==  x']

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let $ (let2int c + n) `mod` 26
          | otherwise = c

encode :: Int -> String -> String
encode n str = [shift n x | x <- str]

table ::  [Float]
table = [8.1, 1.5,  2.8,  4.2,  12.7, 2.2,  2.0,  6.1,  7.0,
           0.2,  0.8,  4.0,  2.4,  6.7,  7.5,  1.9,  0.1,  6.0,
           6.3,  9.0,  2.8,  1.0,  2.4,  0.2,  2.0,  0.1]

percent :: Int -> Int -> Float
percent x y = fromIntegral x / fromIntegral y * 100

freqs :: String -> [Float]
freqs str = [percent (count x str) (lowers str) | x <- ['a'..'z']]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n lst = drop n lst ++ take n lst


crack :: String -> String
crack str = encode (-factor) str
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate i table') table | i <- [0..25]]
    table' = freqs str
