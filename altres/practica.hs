
maximo3 :: Ord a => a -> a -> a -> a
maximo3 x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | otherwise = z

max2 :: Ord p => p -> p -> p
max2 x y
    | x >= y = x
    | otherwise = y

max3 :: Ord p => p -> p -> p -> p
max3 x y z
    | max2 x y >= max2 y z = max2 x y
    | otherwise = max2 y z

max4 :: Ord p => p -> p -> p -> p -> p
max4 x y z t = max2 (max2 x y) (max2 z t)

tresDiferentes :: Eq a => a -> a -> a -> Bool
tresDiferentes x y z = x /= y && y /= z && x /= z

cuatroIguales :: Eq a => a -> a -> a -> a -> Bool
cuatroIguales x y z t = x == y && x == z && x == t && y == z && y == t && z == t

media3 :: Fractional a => a -> a -> a -> a
media3 x y z = (x + y + z) / 3


cuantosSobreMedia3 :: (Num a1, Fractional a2, Ord a2) => a2 -> a2 -> a2 -> a1
cuantosSobreMedia3 x y z = sobreM x + sobreM y + sobreM z
    where
        resultat = media3 x y z
        sobreM w
            | w > resultat = 1
            | otherwise = 0


pp :: (Eq t, Num t, Num p) => p -> t -> p
pp x 0 = 0
pp x y = x + pp x (y-1)

producto :: (Ord p, Ord t, Num t, Num p) => p -> t -> p
producto x y
    | x > 0 && y > 0 || x < 0 && y < 0 = pp absX absY
    | x < 0 && y > 0 || x > 0 && y < 0 = -pp absX absY
    where
        absX = abs x
        absY = abs y


buscarDesde :: (Ord a, Num a) => a -> a -> a
buscarDesde i n
    | i*i > n = i - 1
    | i*i < n = buscarDesde (i + 1) n
    | otherwise = i

raiz :: Integer -> Integer
raiz = buscarDesde 1

opera :: (t -> t -> t) -> [t] -> t
opera _ [] = error "error"
opera f [x] = x
opera f (x:xs) = f x (opera f xs)

opera2 :: (t -> t -> t) -> [t] -> [t]
opera2 _ [] = error "error"
opera2 f [x] = error "Llista imparella"
opera2 f [x,y] = [f x y]
opera2 f (x:y:xs) = f x y:opera2 f xs

filtra :: (a -> Bool) -> [a] -> [a]
filtra _ [] = error "Llista buida"
filtra f [x] = if (f x) then [x] else []
filtra f (x:xs) = if (f x) then x:filtra f xs else filtra f xs

rechaza :: (a -> Bool) -> [a] -> [a]
rechaza _ [] = error "Llista buida"
rechaza f [x] = if not (f x) then [x] else []
rechaza f (x:xs) = if not (f x) then x:rechaza f xs else rechaza f xs


divideA :: Integral a => a -> a -> Bool
divideA x y = mod x y == 0


divisores :: Integral a => a -> [a]
divisores x = filtra (divideA x) [1..x]

mcd :: (Num a, Ord a) => a -> a -> a
mcd x 0 = x
mcd 0 y = y
mcd x y = if (x >= y) then mcd (x-y) y else mcd x (y-x)


-- >>> mcd 10 10
-- 10
--
