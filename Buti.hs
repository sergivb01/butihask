module Buti where

import Debug.Trace (trace)

------------------------------------------------------- TIPUS -------------------------------------------------------

data TipusCarta = Manilla | As | Rei | Cavall | Sota | Vuit | Set | Sis | Cinc | Quatre | Tres | Dos deriving (Read, Show, Enum, Eq, Ord)

data Pal = Oros | Copes | Espases | Bastos deriving (Read, Show, Enum, Eq, Ord)

data Carta = Carta {tipusCarta :: TipusCarta, pal :: Pal} deriving (Read, Eq, Ord)

data Trumfu = Trumfu {palTrumfu :: Pal} | Butifarra deriving (Read, Show, Eq, Ord)

instance Show Carta where
  show (Carta t p)
    | p == Espases || p == Oros = show t ++ " d'" ++ show p
    | otherwise = show t ++ " de " ++ show p

deck :: [Carta]
deck = [Carta val su | su <- [Oros .. Bastos], val <- [Manilla .. Dos]]

---------------------------------------------------- PRINCIPALS ----------------------------------------------------

-- funcio d'immersió per fer la recursivitat de trampa
comprovarBases :: [[Carta]] -> Trumfu -> [[Carta]] -> Int -> Int -> Maybe ([Carta],Int, Int)
comprovarBases _ _ [] _ _ = Nothing
comprovarBases cj t (bAct:xs) p nBasa =
  case bc of
    Just x -> Just (bAct, x, nBasa)
    Nothing -> trace ("Next = " ++ show next ++ ", cjNext=" ++ show cjNext) (comprovarBases cjNext t xs next (nBasa + 1))
  where
    ordreTirada = [seguent x | x <- [p - 1 .. (p + 2)]]
    bc = basaCorrecta cj t p bAct
    next = quiSortira p (snd(quiGuanya bAct t) + 1)
    cjNext = [borrarElement (cj !! x) (bAct !! (posicioLlista ordreTirada (x + 1))) | x <- [0..3]]

-- trampa: Donades les cartes dels quatre jugadors, donat el trumfu (el pal que mana) de la partida,
-- la llista de cartes tirades per ordre (de la primera a la última) i el número de jugador que
-- ha tirat la primera carta, ens retorna (si efectivament hi ha hagut trampa) la basa i el
-- número de basa on s’ha produït la trampa i el jugador que l’ha feta
trampa:: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe ([Carta],Int, Int)
trampa cj t tirades p = comprovarBases cj t bases p 1
  where
    -- cjBasa = [borrarElement (cj !! x) carta | x <- [0..3]]
    -- guanyador = [quiGuanya (bases !! x) t | x <- [0 .. nombreBases - 1]]
    bases = [take 4 (drop (x*4) tirades) | x <- [0 .. nombreBases - 1]]
    nombreBases = length tirades `div` 4

-- funcio d'immersió per fer la recursivitat de cartesGuanyades
guanyadorBases _ [] _ = ([], [])
guanyadorBases t (bAct:xs) p
  | jg == 0 || jg == 2 = (bAct ++ fst gb, snd gb)
  | otherwise = (fst gb, bAct ++ snd gb)
  where
    ordreTirada = [seguent x | x <- [p - 1 .. (p + 2)]]
    jg = ordreTirada !! snd(quiGuanya bAct t) - 1
    next = quiSortira p (snd(quiGuanya bAct t) + 1)
    gb = guanyadorBases t xs next

-- cartesGuanyades: Donat el trumfu de la partida, la llista de cartes tirades per ordre (de la primera a la
-- última) i el número de jugador que ha tirat la primera carta, ens retorna una tupla amb
-- les llistes de cartes guanyades per cada parella
cartesGuanyades:: Trumfu -> [Carta] -> Int -> ([Carta],[Carta])
cartesGuanyades t ct p = guanyadorBases t bases p
  where
    bases = [take 4 (drop (x*4) ct) | x <- [0 .. nombreBases - 1]]
    nombreBases = length ct `div` 4

-- punts: Donada una llista de cartes, en conta els punts que hi ha
punts :: [Carta] -> Int
punts [] = 0
punts (x : xs)
  | p >= 5 = trace ("Punts per " ++ show x ++ "=0") (punts xs)
  | otherwise = trace ("Punts per " ++ show x ++ "=" ++ show (5 - p)) ((5 - p) + punts xs)
  where
    p = fromEnum (tipusCarta x)


-- puntsParelles: Donades les cartes dels quatre jugadors, donat el trumfu de la partida (el pal que mana),
-- la llista de cartes tirades per ordre (de la primera a la última) i el número de jugador que
-- ha tirat la primera carta, ens retorna (si no hi ha hagut trampa) una tupla amb els punts
-- fets per la primera i per la segona parella
puntsParelles:: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe (Int, Int)
puntsParelles cj t tirades p
  | trampa cj t tirades p == Nothing = Just (p1, p2)
  | otherwise = Nothing
  where
    p1 = punts (fst cg) + length(fst cg) `div` 4
    p2 = punts (snd cg) + length(snd cg) `div` 4
    cg = cartesGuanyades t tirades p

---------------------------------------------------- ADICIONALS ----------------------------------------------------

-- cartesPal: donada una llista de cartes i un pal, torni les que són del pal donat
-- utilitza el predicat per filtrar on el pal de la carta és igual al paràmetre
cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal xs p = filter (\c -> pal c == p) xs

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

-- >>> [quiSortira x y | x <- [1..4], y <- [1..4]]
-- [1,2,3,4,2,3,4,1,3,4,1,2,4,1,2,3]

-- >>> quiGuanya [Carta As Oros, Carta Dos Bastos, Carta Dos Bastos, Carta Dos Bastos] Butifarra
-- (As de Oros,0)


-- - El company esta guanyant:
--     - Tinc pal sortida => treure una del pal sortida
--     - No tinc pal sortida => totes
jugadesCompany :: [Carta] -> Trumfu -> [Carta] -> [Carta]
jugadesCompany cartes _ [] = cartes
jugadesCompany cartes t (sortida : xs)
  | hiHaPal cartes (pal sortida) = cartesPal cartes (pal sortida)
  | otherwise = cartes

cartesMaten :: [Carta] -> Carta -> Trumfu -> [Carta]
-- Totes del pal victima superiors a la victma + extres
cartesMaten cartes victima t = filter (\c -> pal c == pal victima && (tipusCarta c < tipusCarta victima)) cartes ++ extres
  where
    extres
      | t == Butifarra || pal victima == palTrumfu t = []
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
    maten = cartesMaten cartes guanyant t

-- jugades: donades les cartes que te un jugador, el pal de la partida i les cartes tirades fins al moment en la basa actual,
-- ens retorni la llista de cartes que pot tirar (d’acord amb les normes del joc)
jugades :: [Carta] -> Trumfu -> [Carta] -> [Carta]
jugades [] _ _ = []
jugades cartes _ [] = cartes
jugades cartes t tirades
  -- falta comprovar si el que esta guanyant es el company o no
  | tirs >= 2 && snd (quiGuanya tirades t) == tirs - 2 = jugadesCompany cartes t tirades -- el company ja ha tirat
  | otherwise = jugadesNoCompany cartes t tirades -- som els primers de la parella en tirar
  where
    tirs = length tirades

existeixLlista :: Eq a => [a] -> a -> Bool
existeixLlista [] _ = False
existeixLlista (x : xs) y
  | x == y = True
  | otherwise = existeixLlista xs y


-- retorna el numero del seguent jugador que tirara a la basa
seguent :: Integral a => a -> a
seguent x = x `mod` 4 + 1

-- basaCorrecta: donades la llista de llistes de cartes dels jugadors, donat el pal de la partida,
-- donat el jugador que ha tirat primer a la basa, i donada la llista de cartes de la basa, ens digui,
-- si hi ha hagut trampa, qui ha fet la trampa
basaCorrecta :: [[Carta]] -> Trumfu -> Int -> [Carta] -> Maybe Int
basaCorrecta cj t p basa
 | existeixLlista tramposos True = trace ("Basa " ++ show basa ++ " incorrecta. cartesJugades=" ++ show cartesJugades ++ ", ordreJugadors=" ++ show ordreJugadors ++ ", tramposos=" ++ show tramposos ++ " | CJ= " ++ show cj) (Just (ordreJugadors !! posicioLlista tramposos True))
 | otherwise = Nothing
  where
    -- les bases en cada canvi començant per llista buida
    bases = [take x basa | x <- [0..length basa]]

    -- llista dels jugadors per ordre de tirada
    ordreJugadors = [seguent x | x <- [p - 1 .. (p + 2)]]

    -- llista de cartes que pot jugar cada jugador ordenat per ordre de tirada
      -- (ordreJugadors !! x) - 1 => ens retorna l'index del jugador que ha tirat al torn x [0..3]
      -- (bases !! x) => ens retorna la basa abans que tiri el jugador actual
    cartesJugades = [jugades (cj !! ((ordreJugadors !! x) - 1)) t (bases !! x) | x <- [0..3]]

    -- llista que ens diu si els jugadors han fet trampes, en ordre de tirada
      -- (cartesJugades !! x) => cartes que podia jugar el jugador
      -- ((bases !! (x + 1)) !! x) => ens diu la carta que ha tirat el jugador
    tramposos = [not (existeixLlista (cartesJugades !! x) ((bases !! (x + 1)) !! x)) | x <- [0..3]]

-- TODO: reemplaçar per un filter
borrarElement :: Eq a => [a] -> a -> [a]
borrarElement xs x = filter (/= x) xs
-- borrarElement [] _ = []
-- borrarElement (x:xs) e
--   | e == x = borrarElement xs e
--   | otherwise = x : borrarElement xs e
