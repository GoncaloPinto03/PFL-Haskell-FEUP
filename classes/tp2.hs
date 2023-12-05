import Data.Char

-- 2.1

-- a)
myand :: [Bool] -> Bool
myand [] = True
myand (False : list) = False
myand (_ : list) = myand list

-- b)
myor :: [Bool] -> Bool
myor [] = False
myor (True : list) = True
myor (_ : list) = myor list

-- c)
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (list : rest) = list ++ myconcat rest
 -- d)
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = (x : myreplicate (n-1) x)

-- e)
(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
xs !!! n | n < 0 = error "Negative Index"
[] !!! _ = error "Index to large"
(_:xs) !!! n = xs !!! (n-1)

-- f)
myelem :: Eq a => a ->[a] -> Bool
myelem _ [] = False
myelem y (x:xs) | y==x = True
                | otherwise = myelem y xs

-- 2.2
myintersperse :: a -> [a] -> [a]
myintersperse _ [] = []
myintersperse _ [x] = [x]
myintersperse y (x:xs) = (x:y : myintersperse y xs)
-- to test myintersperse '-' "banana"

-- 2.4
-- a)
myinsert :: Ord a => a -> [a] -> [a]
myinsert n [] = [n]
myinsert n (x:xs)   | n < x = [n] ++ (x:xs) 
                    | otherwise = (x : myinsert n xs)

-- b)
myisort :: Ord a => [a] -> [a] 
myisort [] = []
myisort (x:xs) = myinsert x (myisort xs)

-- 2.5

-- a)
myminimum :: Ord a => [a] -> a
myminimum xs = case myisort xs of
                 []    -> error "Empty list has no minimum"
                 (y:_) -> y

-- b)
mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete n (x:xs)   | x == n = xs
                    | otherwise = (x : mydelete n xs)

-- c)
myssort :: Ord a => [a] -> [a] 
myssort [] = []
myssort x = myminimum x : myssort(mydelete(myminimum x) x)

-- 2.6)
sum_square :: Integer
sum_square = sum[x^2 | x <- [1..100]]