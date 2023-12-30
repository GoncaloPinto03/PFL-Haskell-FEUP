-- 2.1
myand :: [Bool] -> Bool
myand [] = True
myand (False:list) = False
myand (True:list) = myand list

myor :: [Bool] -> Bool
myor [] = False
myor (True:list) = True
myor(False:list) = myor list

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n a = a : myreplicate (n-1) a

(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem n (x:xs) = if (n==x) then True else myelem n xs

-- 2.2
myintersperse :: a -> [a] -> [a]
myintersperse _ [] = []
myintersperse _ [x] = [x]
myintersperse v (x:xs) = x:v : (myintersperse v xs)

-- 2.3
mdc :: Integer -> Integer -> Integer
mdc a b = if b==0 then a else mdc b (a `mod` b)

-- 2.4
myinsert :: Ord a => a -> [a] -> [a] 
myinsert n [] = [n]
myinsert n (x:xs) = if n<x then n:x:xs else x:myinsert n xs 

myisort :: Ord a => [a] -> [a]
myisort [] = []
myisort (x:xs) = myinsert x (myisort xs)

-- 2.5
myminimum :: Ord a => [a] -> a
myminimum list = head (myisort list)

mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete n (x:xs) = if n == x then xs else x:mydelete n xs

myssort :: Ord a => [a] -> [a] 
myssort [] = []
myssort list = myminimum list : myssort (mydelete (myminimum list) list)

-- 2.6
sumSquare :: Integer
sumSquare = sum[x^2 | x <- [1..100]]

-- 2.7
aprox :: Int -> Double
aprox n = 4 * sum[((-1)^k) / (2 * fromIntegral k + 1) | k <- [0..n]]

aprox2 :: Int -> Double
aprox2 n = sqrt (12 * sum[(-1)^k / (fromIntegral k + 1)^2 | k <- [0..n]])