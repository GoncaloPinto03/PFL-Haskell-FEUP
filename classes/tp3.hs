import Data.List
-- 3.1
-- [f x | x ← xs, p x]
-- map(f (filter p xs))

-- 3.2
dec2int :: [Int] -> Int
dec2int xs = foldl((+) .(*10)) 0 xs
-- dec2int xs = foldl( \x y -> 10*x + y) 0 xs

-- 3.3
--zipWith f xs ys = [f x y | (x, y) <- zip xs ys]
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] [] = []
myZipWith f (x1:x1s) (x2:x2s) = f x1 x2 : myZipWith f x1s x2s

-- 3.4
isort :: Ord a => [a] -> [a]
isort a = foldr insert [] a

-- 3.5
-- a)
myMax :: Ord a => [a] -> a
myMax a = foldl1(max) a

myMin :: Ord a => [a] -> a
myMin a = foldr1(min) a

-- b)
myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 f l = foldl f (head l) (tail l)

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 f l = foldr f (last l) (init l)

-- 3.6
mdc :: Int -> Int -> Int
mdc a b = fst (until (\(a, b) ->  b == 0) (\(a, b) -> (b, a `mod` b)) (a, b))
