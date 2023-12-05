-- 1.1
testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = (a + b) > c && (a + c) > b && (b + c) > a

-- 1.2
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2

-- 1.3
metades :: [list] -> ([list], [list])
metades list = (take halflen list, drop halflen list)
    where halflen = length(list) `div` 2

-- 1.4
my_last :: [a] -> a
my_last l = head(reverse l)
-- or
my_last2 l = head(drop(length l - 1) l)

my_init :: [a] -> [a]
my_init l = reverse(drop 1 (reverse l)) 
-- or
my_init2 l = take(length l - 1) l

-- 1.7
{--
a) [Char, Char, Char] -> ]list of chars
b) (Char, Char, Char) -> tuple of chars
c) [(Bool, Char), (Bool, Char)] -> list of tuples with a bool and a char
d) ([Bool], [Char]) -> tuple of lists
(e) [tail, init, reverse] = [[a] -> [a]]
(f) [id, not] = [Bool -> Bool]
-}

-- 1.8
{--
(a) [a] -> a
(b) (a, b) -> (b, a)
(c) a -> b -> (a, b)
(d) (Num a) => a -> a
(e) (Fractional a) => a -> a
(f) Char -> Bool
(g) Char -> Bool
(h) (Eq a) => [a] -> Bool
(i) (a -> a) => a -> a
-}

-- 1.9
classifica :: Int -> String
classifica n
    | (n <= 9) = "reprovado"
    | (n > 9 && n <= 12) = "suficiente"
    | (n > 12 && n <= 15) = "bom"
    | (n > 15 && n <= 18) = "muito bom"
    | (n > 18 && n <= 20) = "muito bom com distincao"
    | otherwise = "nota invalida"

-- 1.14
-- a)
curta l = if(length l < 3) then True else False
-- or
curta2 l = length l < 3
-- b)
curta3 [] = True
curta3 [_] = True
curta3 [_, _] = True
curta3 _ = False
