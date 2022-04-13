module Buti where

import Debug.Trace (trace)

------------------------------------------------------- TIPUS -------------------------------------------------------

data TipusCarta = Manilla | As | Rei | Cavall | Sota | Vuit | Set | Sis | Cinc | Quatre | Tres | Dos deriving (Read, Show, Enum, Eq, Ord)

data Pal = Oros | Copes | Espases | Bastos deriving (Read, Show, Enum, Eq, Ord)

data Carta = Carta {tipusCarta :: TipusCarta, pal :: Pal} deriving (Read, Eq, Ord)

data Trumfu = Trumfu {palTrumfu :: Pal} | Butifarra deriving (Read, Show, Eq, Ord)

instance Show Carta where
  show (Carta t p) = show t ++ " de " ++ show p

type LlistaCartes = [Carta]

deck :: LlistaCartes
deck = [Carta val su | su <- [Oros .. Bastos], val <- [Manilla .. Dos]]

---------------------------------------------------- PRINCIPALS ----------------------------------------------------
-- trampa: Donades les cartes dels quatre jugadors, donat el trumfu (el pal que mana) de la partida,
-- la llista de cartes tirades per ordre (de la primera a la última) i el número de jugador que
-- ha tirat la primera carta, ens retorna (si efectivament hi ha hagut trampa) la basa i el
-- número de basa on s’ha produït la trampa i el jugador que l’ha feta
-- trampa:: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe ([Carta],Int, Int)

-- cartesGuanyades: Donat el trumfu de la partida, la llista de cartes tirades per ordre (de la primera a la
-- última) i el número de jugador que ha tirat la primera carta, ens retorna una tupla amb
-- les llistes de cartes guanyades per cada parella
-- cartesGuanyades:: Trumfu -> [Carta] -> Int -> ([Carta],[Carta])

-- punts: Donada una llista de cartes, en conta els punts que hi ha
punts :: [Carta] -> Int
punts [] = 0
punts (x : xs)
  | p >= 5 = 0 + punts xs
  | otherwise = 5 - p + punts xs
  where
    p = fromEnum (tipusCarta x)

-- puntsParelles: Donades les cartes dels quatre jugadors, donat el trumfu de la partida (el pal que mana),
-- la llista de cartes tirades per ordre (de la primera a la última) i el número de jugador que
-- ha tirat la primera carta, ens retorna (si no hi ha hagut trampa) una tupla amb els punts
-- fets per la primera i per la segona parella
-- puntsParelles:: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe (Int, Int)
-- TODO: si es butifarra, la puntuació es doble

---------------------------------------------------- ADICIONALS ----------------------------------------------------

-- cartesPal: donada una llista de cartes i un pal, torni les que són del pal donat
-- utilitza el predicat per filtrar on el pal de la carta és igual al paràmetre
cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal xs p = filter (\c -> pal c == p) xs

-- >>> cartesPal [Carta As Oros] Butifarra
-- [As de Oros]

-- retorna cert si en la llista de cartes hi ha alguna que és del Pal del paràmetre
hiHaPal :: [Carta] -> Pal -> Bool
hiHaPal xs p = length (cartesPal xs p) > 0 -- també podriem cridar cartesPal i comprovar length

-- palGuanyadorBasa: donada una llista de cartes (en ordre de tirada), i el pal del trumfu, ens indiqui quin és el pal que està guanyant la basa
-- TODO: es pot passar a foldr (?)
palGuanyadorBasa :: [Carta] -> Trumfu -> Pal
palGuanyadorBasa [] _ = error "llista buida"
palGuanyadorBasa (x : xs) t
  | t == Butifarra = pal x
  | hiHaPal (x : xs) (palTrumfu t) = palTrumfu t
  | otherwise = pal x

-- posicioLlista: donada una llista i un element, retorna la posició de l'element
posicioLlista :: Eq a => [a] -> a -> Int
posicioLlista [] _ = error "llista buida o element no hi pertany"
posicioLlista (x : xs) y
  | x == y = 0
  | otherwise = 1 + posicioLlista xs y

-- quiGuanya: donada una llista de cartes (en ordre de tirada) i el pal del trumfu,
-- ens retorni la carta guanyadora i la seva posició a la llista
-- TODO: fer una reullada?
quiGuanya :: [Carta] -> Trumfu -> (Carta, Int)
quiGuanya xs t = (c, posicioLlista xs c)
  where
    c = minimum (cartesPal xs (palGuanyadorBasa xs t))

-- quiSortira: donat el nombre de jugador que ha començat la basa, i la posició del que ha guanyat la basa, ens digui quin és el nombre de jugador que començarà la següent basa
-- (és a dir, el que ha guanyat la basa actual). Per exemple, si ha començat tirant el jugador 2 i guanya la basa el que ha tirat segon, començarà la següent basa el jugador 3
quiSortira :: Int -> Int -> Int
quiSortira x y
  | x + y - 1 > 4 = x + y - 5
  | otherwise = x + y - 1


-- - El company esta guanyant:
--     - Tinc pal sortida => treure una del pal sortida
--     - No tinc pal sortida => totes
jugadesCompany :: [Carta] -> Trumfu -> [Carta] -> [Carta]
jugadesCompany cartes _ [] = cartes
jugadesCompany cartes t (sortida : xs)
  | hiHaPal cartes (pal sortida) = cartesPal cartes (pal sortida)
  | otherwise = cartes

-- cartesMaten: donada una llista de cartes i una que volem matar (la que va guanyant), retornem les que poden matar-la
-- i són del mateix pal. Sense tenir en compte el trumfu
cartesMaten :: [Carta] -> Carta -> Trumfu -> [Carta]
cartesMaten cartes victima t
-- Si el pal de la carta victma és igual al pal del Trumfu => Totes del pal victima superiors a la victma
 | t == Butifarra || pal victima == palTrumfu t = mateixPal
-- Si el pal de la carta victma és diferent al pal del Trumfu => Totes les del pal Trumfu + Totes del pal victima superiors a la victma
 | otherwise = mateixPal ++ filter (\c -> pal c == palTrumfu t) cartes
 where mateixPal = filter (\c -> pal c == pal victima && (tipusCarta c < tipusCarta victima)) cartes

cartesMaten2 :: [Carta] -> Carta -> Trumfu -> [Carta]
-- totes del pal victima superiors a la victma + extres
cartesMaten2 cartes victima t = filter (\c -> pal c == pal victima && (tipusCarta c < tipusCarta victima)) cartes ++ extres
  where extres | t == Butifarra || pal victima == palTrumfu t =  []
              | otherwise = filter (\c -> pal c == palTrumfu t) cartes -- afegim també les que són del Trumfu

-- - El company NO esta guanyant:
--    - Tinc pal sortida => jugar pal sortida (i si puc, matar-la)
--    - No tinc pal sortida però tinc guanya la basa (triomf) => Les que guanyin la basa
--    - No tinc pal sortida ni guanya basa => Totes
jugadesNoCompany :: [Carta] -> Trumfu -> [Carta] -> [Carta]
jugadesNoCompany [] _ _ = []
jugadesNoCompany _ _ [] = []
jugadesNoCompany cartes t (sortida : xs)
  -- Si el jugador té cartes del pal de sortida haurà de jugar-ne una d'aquest pal i sempre que pugui haurà de matar-la.
  | hiHaPal cartes (pal sortida) && hiHaPal maten (pal sortida) = cartesPal maten (pal sortida)
  | hiHaPal cartes (pal sortida) = cartesPal cartes (pal sortida)
  -- Si el jugador no té cap carta del pal de sortida però en té d'altres que guanyen la basa (triomf) haurà de jugar una carta que guanyi la basa.
  | length maten > 0 = maten
  -- Si el jugador no té ni cap carta del pal de sortida ni cap carta que guanyi la basa, podrà jugar la carta que vulgui.
  | otherwise = cartes
  where
    guanyant = fst (quiGuanya (sortida : xs) t)
    maten = cartesMaten2 cartes guanyant t

-- jugades: donades les cartes que te un jugador, el pal de la partida i les cartes tirades fins al moment en la basa actual,
-- ens retorni la llista de cartes que pot tirar (d’acord amb les normes del joc)
jugades :: [Carta] -> Trumfu -> [Carta] -> [Carta]
jugades [] _ _ = []
jugades cartes _ [] = cartes
jugades cartes t tirades
  -- falta comprovar si el que esta guanyant es el company o no
  | tirs >= 2 && snd (quiGuanya tirades t) == tirs = jugadesCompany cartes t tirades -- el company ja ha tirat
  | otherwise = jugadesNoCompany cartes t tirades -- som els primers de la parella en tirar
  where
    tirs = length tirades

-- >>> length [1,2,3,4,5,6]
-- 6

-- basaCorrecta: donades la llista de llistes de cartes dels jugadors, donat el pal de la partida, donat el jugador que ha tirat primer a la basa, i donada la llista de cartes de la basa, ens digui, si hi ha hagut trampa, qui ha fet la trampa
