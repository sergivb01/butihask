-- data Pal = Oros | Copes | Espases | Bastos deriving (Show)
-- data TipusCarta = As | Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Manilla | Sota | Cavall | Rei deriving (Show)
-- data Carta = Carta TipusCarta Pal deriving (Show)

-- a :: Carta
-- a = Carta As Oros

-- ab :: Carta -> TipusCarta
-- ab (Carta f g) = f

-- bc :: Carta -> Pal
-- bc (Carta f g) = g

-- -- >>> a
-- --- >>> ab a
-- -- >>> bc a
-- -- Carta As Oros
-- -- As
-- -- Oros

data TipusCarta = As | Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Manilla | Sota | Cavall | Rei deriving (Read, Show, Enum, Eq, Ord)

data Pal = Oros | Copes | Espases | Bastos deriving (Read, Show, Enum, Eq, Ord)

data Carta = Carta {tipusCarta :: TipusCarta, pal :: Pal} deriving (Read, Eq)

data Trumfu = Pal | Butifarra deriving (Read, Show, Enum, Eq, Ord)

instance Show Carta where
  show (Carta t p) = show t ++ " de " ++ show p

type Deck = [Carta]

deck :: Deck
deck = [Carta val su | su <- [Oros .. Bastos], val <- [As .. Rei]]

-- cartesPal :: [Carta] -> Pal -> [Carta]
-- cartesPal [] _ = []
-- cartesPal (x : xs) p
--   | pal x == p = x : cartesPal xs p
--   | otherwise = cartesPal xs p

cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal xs p = filter (\c -> pal c == p) xs

-- palGuanyadorBase :: [Carta] -> Pal -> Pal

-- >>> cartesPal deck Copes
-- >>> cartesPal2 deck Copes
-- >>> deck
-- [As de Copes,Dos de Copes,Tres de Copes,Quatre de Copes,Cinc de Copes,Sis de Copes,Set de Copes,Vuit de Copes,Manilla de Copes,Sota de Copes,Cavall de Copes,Rei de Copes]
-- [As de Copes,Dos de Copes,Tres de Copes,Quatre de Copes,Cinc de Copes,Sis de Copes,Set de Copes,Vuit de Copes,Manilla de Copes,Sota de Copes,Cavall de Copes,Rei de Copes]
-- [As de Oros,Dos de Oros,Tres de Oros,Quatre de Oros,Cinc de Oros,Sis de Oros,Set de Oros,Vuit de Oros,Manilla de Oros,Sota de Oros,Cavall de Oros,Rei de Oros,As de Copes,Dos de Copes,Tres de Copes,Quatre de Copes,Cinc de Copes,Sis de Copes,Set de Copes,Vuit de Copes,Manilla de Copes,Sota de Copes,Cavall de Copes,Rei de Copes,As de Espases,Dos de Espases,Tres de Espases,Quatre de Espases,Cinc de Espases,Sis de Espases,Set de Espases,Vuit de Espases,Manilla de Espases,Sota de Espases,Cavall de Espases,Rei de Espases,As de Bastos,Dos de Bastos,Tres de Bastos,Quatre de Bastos,Cinc de Bastos,Sis de Bastos,Set de Bastos,Vuit de Bastos,Manilla de Bastos,Sota de Bastos,Cavall de Bastos,Rei de Bastos]

test :: [Int]
test = [1, 2, 3, 4, 5, 6, 7, 8, 9]

-- >>> foldr (\acc y -> acc + y) 0 test
-- 45

-- >>> [((Trumfu Oros) :: Trumfu) ..]
-- Data constructor not in scope: Trumfu :: Pal -> Trumfu

-- palGuanyadorBasa xs Butifarra = Oros
-- palGuanyadorBasa xs t = t :: Maybe Pal
-- palGuanyadorBasa xs t = foldr (\acc c -> if pal acc == t then t else t) Bastos xs

