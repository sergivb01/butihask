-- PRÀCTICA 2
-- Exercici 1
opera :: (a -> a -> a) -> [a] -> a
opera _ [] = error "Llista buida"
-- També es pot fer el cas base amb dos elements
opera f [x] = x
opera f (x : xs) = f x (opera f xs)
-- >>> opera (*) [1, 2, 3, 4]
-- 24

-- Exercici 2
-- Fixar-nos en els tipus!
opera2 :: (a -> a -> b) -> [a] -> [b]
opera2 _ [] = []
opera2 _ [x] = error "Imparells"
opera2 f (x : y : xs) = f x y : opera2 f xs
-- >>> opera2 (+) [1,2, 3]
-- >>> opera2 (+) [1,2,3,4,5,6]
-- Imparells
-- [3,7,11]

-- Exercici 3
filtra :: (Integer -> Bool) -> [Integer] -> [Integer]
filtra f [] = []
filtra f (x : xs)
 | f x = x : filtra f xs
 | otherwise = filtra f xs
-- >>> filtra even [0,1,2,0,3,4]
-- >>> filtra (> 1) [0,1,2,0,3,4]
-- [0,2,0,4]
-- [2,3,4]

-- Exercici 4
rebutja :: (Integer -> Bool) -> [Integer] -> [Integer]
-- Composició de funcions
rebutja f = filtra (not . f)
-- >>> rebutja even [0,1,2,0,3,4]
-- [1,3]

-- Exercici 5
divideixA :: Integer -> Integer -> Bool
divideixA x y = mod x y == 0
-- >>> divideixA 10 2
-- >>> divideixA 10 3
-- True
-- False

-- Exercici 6
divisors :: Integer -> [Integer]
divisors x = filtra (divideixA x) [1..x]
-- >>> divisors 10
-- [1,2,5,10]

-- Exercici 7
mcd :: Integer -> Integer -> Integer
mcd x y
 | x == 0 = y
 | y == 0 = x
 | x > y = mcd (x - y) y
 | otherwise = mcd (y - x) x
-- >>> mcd 10 2
-- 2