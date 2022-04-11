module Butifarra where

------------------------------------------------------- TIPUS -------------------------------------------------------

data TipusCarta = As | Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Manilla | Sota | Cavall | Rei deriving (Read, Show, Enum, Eq, Ord)

data Pal = Oros | Copes | Espases | Bastos deriving (Read, Show, Enum, Eq, Ord)

data Carta = Carta {tipusCarta :: TipusCarta, pal :: Pal} deriving (Read, Eq, Ord)

data Trumfu = Trumfu {palTrumfu :: Pal} | Butifarra deriving (Read, Show, Eq, Ord)

instance Show Carta where
  show (Carta t p) = show t ++ " de " ++ show p

type LlistaCartes = [Carta]

deck :: LlistaCartes
deck = [Carta val su | su <- [Oros .. Bastos], val <- [As .. Rei]]

---------------------------------------------------- PRINCIPALS ----------------------------------------------------

-- trampa:: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe ([Carta],Int, Int)

-- cartesGuanyades:: Trumfu -> [Carta] -> Int -> ([Carta],[Carta])

-- punts:: [Carta] -> Int

-- puntsParelles:: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe (Int, Int)

---------------------------------------------------- ADICIONALS ----------------------------------------------------

-- cartesPal: donada una llista de cartes i un pal, torni les que són del pal donat
-- utilitza el predicat per filtrar on el pal de la carta és igual al paràmetre
cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal xs p = filter (\c -> pal c == p) xs

hiHaPal :: [Carta] -> Pal -> Bool
hiHaPal [] _ = False
hiHaPal (x : xs) p
  | pal x == p = True
  | otherwise = hiHaPal xs p

-- palGuanyadorBasa: donada una llista de cartes (en ordre de tirada), i el pal del trumfu, ens indiqui quin és el pal que està guanyant la basa
palGuanyadorBasa :: [Carta] -> Trumfu -> Pal
palGuanyadorBasa [] _ = error "llista buida"
palGuanyadorBasa (x : xs) t
  | t == Butifarra = pal x
  | hiHaPal (x : xs) (palTrumfu t) = palTrumfu t
  | otherwise = pal x

-- quiSortira: e donat el nombre de jugador que ha començat la basa, i la posició del que ha guanyat la basa, ens digui quin és el nombre de jugador que començarà la següent basa

-- (´es a dir, el que ha guanyat la basa actual). Per exemple, si ha començat tirant el jugador 2 i guanya la basa el que ha tirat segon, començarà la següent basa el jugador 3

-- jugades: e donades les cartes que te un jugador, el pal de la partida i les cartes tirades fins al moment en la basa actual, ens retorni la llista de cartes que pot tirar (d’acord amb les normes del joc)

-- basaCorrecta: donades la llista de llistes de cartes dels jugadors, donat el pal de la partida, donat el jugador que ha tirat primer a la basa, i donada la llista de cartes de la basa, ens digui, si hi ha hagut trampa, qui ha fet la trampa
