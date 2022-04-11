module Buti where

------------------------------------------------------- TIPUS -------------------------------------------------------

data TipusCarta = Manilla | As | Rei | Cavall | Sota | Dos | Tres | Quatre | Cinc | Sis | Set | Vuit deriving (Read, Show, Enum, Eq, Ord)

data Pal = Oros | Copes | Espases | Bastos deriving (Read, Show, Enum, Eq, Ord)

data Carta = Carta {tipusCarta :: TipusCarta, pal :: Pal} deriving (Read, Eq, Ord)

data Trumfu = Trumfu {palTrumfu :: Pal} | Butifarra deriving (Read, Show, Eq, Ord)

instance Show Carta where
  show (Carta t p) = show t ++ " de " ++ show p

type LlistaCartes = [Carta]

deck :: LlistaCartes
deck = [Carta val su | su <- [Oros .. Bastos], val <- [As .. Rei]]

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
-- punts:: [Carta] -> Int

-- puntsParelles: Donades les cartes dels quatre jugadors, donat el trumfu de la partida (el pal que mana),
-- la llista de cartes tirades per ordre (de la primera a la última) i el número de jugador que
-- ha tirat la primera carta, ens retorna (si no hi ha hagut trampa) una tupla amb els punts
-- fets per la primera i per la segona parella
-- puntsParelles:: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe (Int, Int)

---------------------------------------------------- ADICIONALS ----------------------------------------------------

-- cartesPal: donada una llista de cartes i un pal, torni les que són del pal donat
-- utilitza el predicat per filtrar on el pal de la carta és igual al paràmetre
cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal xs p = filter (\c -> pal c == p) xs


-- retorna cert si en la llista de cartes hi ha alguna que és del Pal del paràmetre
hiHaPal :: [Carta] -> Pal -> Bool
hiHaPal xs p = any (\c -> pal c == p) xs


-- palGuanyadorBasa: donada una llista de cartes (en ordre de tirada), i el pal del trumfu, ens indiqui quin és el pal que està guanyant la basa
-- TODO: es pot passar a foldr (?)
palGuanyadorBasa :: [Carta] -> Trumfu -> Pal
palGuanyadorBasa [] _ = error "llista buida"
palGuanyadorBasa (x : xs) t
  | t == Butifarra = pal x
  | hiHaPal (x : xs) (palTrumfu t) = palTrumfu t
  | otherwise = pal x


posicioLlista :: Eq a => [a] -> a -> Int
posicioLlista [] _ = error "llista buida"
posicioLlista (x : xs) y
 | x == y = 0
 | otherwise = 1 + posicioLlista xs y

-- quiGuanya: donada una llista de cartes (en ordre de tirada) i el pal del trumfu,
-- ens retorni la carta guanyadora i la seva posició a la llista
-- TODO: fer una reullada?
quiGuanya :: [Carta] -> Trumfu -> (Carta, Int)
quiGuanya xs t = (c, posicioLlista xs c)
  where c = minimum (cartesPal xs (palGuanyadorBasa xs t))


-- >>> quiGuanya [Carta Vuit Bastos, Carta As Oros, Carta Manilla Oros, Carta Cavall Bastos] Butifarra
-- (Cavall de Bastos,3)
-- >>> length [Carta Vuit Bastos, Carta As Oros, Carta Manilla Oros]
-- 3


-- quiGuanya (x : y : xs) t
--  | tipusCarta x > tipusCarta (quiGuanya xs t) = quiGuanya xs t
--  | == palGuanyador = (x, 1)
--  where palGuanyador = palGuanyadorBasa (x:xs) t

-- quiSortira: donat el nombre de jugador que ha començat la basa, i la posició del que ha guanyat la basa, ens digui quin és el nombre de jugador que començarà la següent basa
-- (és a dir, el que ha guanyat la basa actual). Per exemple, si ha començat tirant el jugador 2 i guanya la basa el que ha tirat segon, començarà la següent basa el jugador 3

-- jugades: donades les cartes que te un jugador, el pal de la partida i les cartes tirades fins al moment en la basa actual, ens retorni la llista de cartes que pot tirar (d’acord amb les normes del joc)

-- basaCorrecta: donades la llista de llistes de cartes dels jugadors, donat el pal de la partida, donat el jugador que ha tirat primer a la basa, i donada la llista de cartes de la basa, ens digui, si hi ha hagut trampa, qui ha fet la trampa
