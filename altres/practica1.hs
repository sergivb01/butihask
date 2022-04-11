-- PRÀCTICA 1

-- Exercici 1
infixr 2 |||
(|||) :: Bool -> Bool -> Bool
True ||| True = False
False ||| False = False
_ ||| _ = True
-- >>> True ||| False ||| True
-- False

-- Exercici extra
infixl 3 !&&
(!&&) :: Bool -> Bool -> Bool
True !&& True = False
_ !&& _ = True
-- >>> True !&& True !&& False
-- True

-- Exercici 2
infixr 2 |||#
(|||#) :: Bool -> Bool -> Bool
x |||# y = x == not y
-- >>> True |||# False |||# True
-- False

-- Exercici 3
maxim3cutre :: Integer -> Integer -> Integer -> Integer
maxim3cutre x y z
 | x > y && x > z = x
 | x < y && y > z = y
 | otherwise      = z
-- >>> maxim3cutre 6 4 1
-- >>> maxim3cutre 6 6 6
-- >>> maxim3cutre 2 6 6
-- >>> maxim3cutre 2 2 6
-- >>> maxim3cutre 6 6 2
-- 6
-- 6
-- 6
-- 6
-- 2

-- Exercici 3 i 4
maxim2 :: Integer -> Integer -> Integer
maxim2 x y
 | x > y = x
 | otherwise = y
-- >>> maxim2 6 4
-- >>> maxim2 6 6
-- >>> maxim2 4 6
-- 6
-- 6
-- 6

maxim3 :: Integer -> Integer -> Integer -> Integer
maxim3 x y z = maxim2 (maxim2 x y) z
-- >>> maxim3 6 4 1
-- >>> maxim3 6 6 6
-- >>> maxim3 2 6 6
-- >>> maxim3 2 2 6
-- >>> maxim3 6 6 2
-- 6
-- 6
-- 6
-- 6
-- 6

maxim4 :: Integer -> Integer -> Integer -> Integer -> Integer
maxim4 x y z p = maxim2 (maxim2 x y) (maxim2 z p)
-- >>> maxim4 10 6 3 4
-- 10

-- Exercici 5
dosDiferents :: Integer -> Integer -> Bool
dosDiferents x y = x /= y
-- >>> dosDiferents 10 10
-- False

tresDiferents :: Integer -> Integer -> Integer -> Bool
tresDiferents x y z = dosDiferents x y && dosDiferents x z && dosDiferents y z
-- >>> tresDiferents 1 102 1023
-- True

-- Exercici 6
quatreDiferents :: Integer -> Integer -> Integer -> Integer -> Bool
quatreDiferents x y z r = tresDiferents x y z && tresDiferents x y r && tresDiferents y z r
-- >>> quatreDiferents 1 4 3 4
-- False