-- 1.1
testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = a < (b + c) && b < (a + c) && c < (a + b)

-- 1.2
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt (((a+b+c)/2)*(((a+b+c)/2)-a)*(((a+b+c)/2)-b)*(((a+b+c)/2)-c))

-- 1.3
metades :: [list] -> ([list], [list])
metades list = ( take s list, drop s list) where s = length list  `div` 2

-- 1.4 
mylast1 :: [a] -> a
mylast1 list = head(reverse list)
mylast2 :: [a] -> a
mylast2 list = head(drop(length list - 1) list)

myinit1 :: [a] -> [a]
myinit1 list = take (length list - 1) list

-- 1.5 a)
binom :: Integer -> Integer -> Integer
binom n k = product [1..n] `div` (product [1..k] * product [1..(n-k)])

-- 1.5 b)
binom2 :: Integer -> Integer -> Integer
binom2 n k  | k < n-k = product [n-k+1..n] `div` product [1..k]
            | otherwise = product [k+1..n] `div` product [1..n-k]

-- 1.6
raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = ((-b + aux) / (2*a), (-b - aux) / (2*a) ) where aux = sqrt(b^2-4*a*c)

-- 1.7
-- a) [Char, Char, Char]
-- b) (Char, Char, Char)
-- c) [(Bool, Char), (Bool, Char)]
-- d) ([Bool, Bool], [Char, Char])
-- e) 
-- f)

-- 1.8
-- a) [a] -> a
-- b) (a, b) -> (b, a)
-- c) a -> b -> (a, b)
-- d) (Num a) => a -> a
-- e) (Fractional a) => a -> a
-- f) Char -> Bool
-- g) Char -> Bool
-- h) (Eq a) => [a] -> Bool
-- i) (a -> a) => a -> a

-- 1.9
classifica :: Int -> String
classifica n    | n >= 19 = "muito bom com distincao"
                | n >= 16 = "muito bom"
                | n >= 13 = "bom"
                | n >= 10 = "suficiente"
                | n <= 9 = "reprovado"

-- 1.10
classificaIMC :: Float -> Float -> String
classificaIMC p a   | imc >= 30 = "obesidade"
                    | imc >= 25 = "excesso de peso"
                    | imc >= 18.5 = "peso normal"
                    | imc < 18.5 = "baixo peso"
                    where imc = p/a^2

-- 1.11
max3 :: Int -> Int -> Int -> Int
max3 a b c  | a > b && a > c = a
            | b > a && b > c = b
            | c > b && c > a = c

max3_2 :: Int -> Int -> Int -> Int
max3_2 a b c    | max a b > c = max a b
                | max a b < c = c

-- 1.12
xor :: Bool -> Bool -> Bool
xor a b | a /= b = True
        | otherwise = False

-- 1.13
safetail :: [a] -> [a] 
safetail [] = []
safetail list = tail list

safetail2 :: [a] -> [a] 
safetail2 list = if length list == 0 then [] else tail list

safetail3 :: [a] -> [a] 
safetail3 list  | length list == 0 = []  
                | otherwise = tail list

-- 1.14
curta :: [a] -> Bool
curta list  | length list <= 2 = True    
            | otherwise = False

-- 1.15
mediana :: Int -> Int -> Int -> Int
mediana a b c   | a >= b && a >= c && c >= b = c
                | a >= b && a >= c && c < b = b
                | a > b && a <= c = a
                | a < b && a > c = a
                | a < b && a < c && b >= c = c
                | a < b && a < c && b < c = b

mediana2 :: Int -> Int -> Int -> Int
mediana2 a b c = a + b + c - min3 - max3 
        where 
        min3 = min a (min b c) 
        max3 = max a (max b c)
                
                