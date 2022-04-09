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
data Carta = Carta {tipusCarta :: TipusCarta, pal :: Pal} deriving (Read, Show, Eq)
data Trumfu = Pal | Butifarra


type Deck = [Carta]

deck::Deck
deck = [Carta val su | su <- [Oros .. Bastos], val <- [As .. Rei]]

cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal [] _ = []
cartesPal (x : xs) p
    | pal x == p = x : cartesPal xs p
    | otherwise = cartesPal xs p

-- palGuanyadorBase :: [Carta] -> Pal -> Pal

 
-- >>> cartesPal deck Copes
-- >>> deck
-- [Carta {tipusCarta = As, pal = Copes},Carta {tipusCarta = Dos, pal = Copes},Carta {tipusCarta = Tres, pal = Copes},Carta {tipusCarta = Quatre, pal = Copes},Carta {tipusCarta = Cinc, pal = Copes},Carta {tipusCarta = Sis, pal = Copes},Carta {tipusCarta = Set, pal = Copes},Carta {tipusCarta = Vuit, pal = Copes},Carta {tipusCarta = Manilla, pal = Copes},Carta {tipusCarta = Sota, pal = Copes},Carta {tipusCarta = Cavall, pal = Copes},Carta {tipusCarta = Rei, pal = Copes}]
-- [Carta {tipusCarta = As, pal = Oros},Carta {tipusCarta = Dos, pal = Oros},Carta {tipusCarta = Tres, pal = Oros},Carta {tipusCarta = Quatre, pal = Oros},Carta {tipusCarta = Cinc, pal = Oros},Carta {tipusCarta = Sis, pal = Oros},Carta {tipusCarta = Set, pal = Oros},Carta {tipusCarta = Vuit, pal = Oros},Carta {tipusCarta = Manilla, pal = Oros},Carta {tipusCarta = Sota, pal = Oros},Carta {tipusCarta = Cavall, pal = Oros},Carta {tipusCarta = Rei, pal = Oros},Carta {tipusCarta = As, pal = Copes},Carta {tipusCarta = Dos, pal = Copes},Carta {tipusCarta = Tres, pal = Copes},Carta {tipusCarta = Quatre, pal = Copes},Carta {tipusCarta = Cinc, pal = Copes},Carta {tipusCarta = Sis, pal = Copes},Carta {tipusCarta = Set, pal = Copes},Carta {tipusCarta = Vuit, pal = Copes},Carta {tipusCarta = Manilla, pal = Copes},Carta {tipusCarta = Sota, pal = Copes},Carta {tipusCarta = Cavall, pal = Copes},Carta {tipusCarta = Rei, pal = Copes},Carta {tipusCarta = As, pal = Espases},Carta {tipusCarta = Dos, pal = Espases},Carta {tipusCarta = Tres, pal = Espases},Carta {tipusCarta = Quatre, pal = Espases},Carta {tipusCarta = Cinc, pal = Espases},Carta {tipusCarta = Sis, pal = Espases},Carta {tipusCarta = Set, pal = Espases},Carta {tipusCarta = Vuit, pal = Espases},Carta {tipusCarta = Manilla, pal = Espases},Carta {tipusCarta = Sota, pal = Espases},Carta {tipusCarta = Cavall, pal = Espases},Carta {tipusCarta = Rei, pal = Espases},Carta {tipusCarta = As, pal = Bastos},Carta {tipusCarta = Dos, pal = Bastos},Carta {tipusCarta = Tres, pal = Bastos},Carta {tipusCarta = Quatre, pal = Bastos},Carta {tipusCarta = Cinc, pal = Bastos},Carta {tipusCarta = Sis, pal = Bastos},Carta {tipusCarta = Set, pal = Bastos},Carta {tipusCarta = Vuit, pal = Bastos},Carta {tipusCarta = Manilla, pal = Bastos},Carta {tipusCarta = Sota, pal = Bastos},Carta {tipusCarta = Cavall, pal = Bastos},Carta {tipusCarta = Rei, pal = Bastos}]
