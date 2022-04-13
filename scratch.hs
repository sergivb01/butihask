import Buti
import Debug.Trace (trace)

---------------------------------------------------- PRINCIPALS ----------------------------------------------------

-- >>> punts deck
-- 60

-- >>> punts [Carta Manilla Bastos, Carta As  Bastos,Carta  Rei  Bastos, Carta Cavall Bastos ,Carta  Sota Bastos ,Carta  Vuit Bastos , Carta Set  Bastos, Carta Sis  Bastos, Carta Cinc  Bastos, Carta Quatre  Bastos, Carta Tres Bastos , Carta Dos Bastos]
-- 15

---------------------------------------------------- ADICIONALS ----------------------------------------------------
-- >>> palGuanyadorBasa [Carta As Bastos, Carta As Oros,Carta As Bastos] (Trumfu Oros)
-- >>> palGuanyadorBasa [Carta As Bastos, Carta As Oros,Carta As Bastos] Butifarra
-- Oros
-- Bastos

-- palGuanyadorBasa2 :: [Carta] -> Trumfu -> Pal
-- palGuanyadorBasa2 [] _ = error "llista buida"
-- palGuanyadorBasa2 (x : xs) t
--   | t == Butifarra = pal x
--   | otherwise = Oros
-- --   | otherwise = foldr (\a b -> b) (pal x) xs

-- >>> quiGuanya [Carta As Bastos, Carta As Oros,Carta As Bastos] (Trumfu Oros)
-- As de Oros

a = [Carta As Bastos, Carta As Oros, Carta As Espases]

-- >>> a !! posicioLlista a (Carta As Espases)
-- >>> posicioLlista a (Carta As Espases)
-- As de Espases
-- 2

-- >>> quiGuanya [Carta Vuit Bastos, Carta As Oros, Carta Manilla Oros, Carta Cavall Bastos] Butifarra
-- (Cavall de Bastos,3)

-- >>> length [Carta Vuit Bastos, Carta As Oros, Carta Manilla Oros]
-- 3

--    9       1    12     11   10
--  Manilla | As | Rei | Cavall | Sota

--            guanyador        company          enemigo
tirades = [Carta Cavall Oros, Carta As Oros, Carta Cavall Bastos]

tenim = [Carta As Espases, Carta Vuit Espases, Carta Manilla Copes, Carta Quatre Bastos]


-- >>> quiGuanya tirades t
-- >>> jugades tenim t tirades
-- (Cavall de Bastos,2)
-- [As de Espases,Vuit de Espases,Manilla de Copes,Quatre de Bastos]

-- >>> [Carta val su | su <- [Oros .. Bastos], val <- [Manilla .. Dos]]

-- >>> [quiSortira x y | x <- [1 .. 4], y <- [1 .. 4]]
-- [1,2,3,4,2,3,4,1,3,4,1,2,4,1,2,3]

t = Butifarra
cjAa :: [[Carta]]
cjAa =
  [ [Carta Dos Oros, Carta Quatre Bastos],
    [Carta Manilla Copes, Carta As Copes, Carta Set Oros],
    [Carta Manilla Espases, Carta Vuit Bastos],
    [Carta Manilla Bastos, Carta Cavall Oros]
  ]

basaAa :: [Carta]
basaAa = [Carta Dos Oros, Carta Set Oros, Carta Vuit Bastos, Carta Cavall Oros]

p = 1::Int

-- basaCorrecta :: [[Carta]] -> Trumfu -> Int -> [Carta]
-- >>> basaCorrecta cjAa t p basaAa
-- Nothing


-- cjAa = cartesJugadors
-- p = 1::Int
-- basesAa = [take x basaAa | x <- [0..length basaAa]]
-- ordreJugadorsAa = [seguent x | x <- [p - 1 .. (p + 2)]]

seguentAA x = x `mod` 4 + 1
basesAa = [take x basaAa | x <- [0..length basaAa]]
ordreJugadorsAa = [seguentAA x | x <- [p - 1 .. (p + 2)]]
cartesJugadesAa = [jugades (cjAa !! ((ordreJugadorsAa !! x) - 1)) t (basesAa !! x) | x <- [0..3]]
trampososAa = [not (existeixLlista (cartesJugadesAa !! x) ((basesAa !! (x + 1)) !! x)) | x <- [0..3]]

-- >>> basesAa
-- >>> ordreJugadorsAa
-- >>> cartesJugadesAa
-- >>> trampososAa
-- [[],[Dos de Oros],[Dos de Oros,Manilla de Copes],[Dos de Oros,Manilla de Copes,Vuit de Bastos],[Dos de Oros,Manilla de Copes,Vuit de Bastos,Cavall de Oros]]
-- [2,3,4,1]
-- [[Manilla de Copes,As de Copes,Set de Oros],[Manilla de Espases,Vuit de Bastos],[Cavall de Oros],[Dos de Oros]]
-- [True,True,True,True]

