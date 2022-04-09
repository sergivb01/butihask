-- Tipus per definir: Pal, TipusCarta, Carta
data TipusCarta = As | Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Manilla | Sota | Cavall | Rei deriving (Read, Show, Enum, Eq, Ord)
data Pal = Oros | Copes | Espases | Bastos deriving (Read, Show, Enum, Eq, Ord)
data Carta = Carta {tipusCarta :: TipusCarta, pal :: Pal} deriving (Read, Show, Eq)


type Deck = [Carta]

deck::Deck
deck = [Carta val su | val <- [As .. Rei], su <- [Oros .. Bastos]]
-- >>> deck
-- [Carta {tipusCarta = As, pal = Oros},Carta {tipusCarta = As, pal = Copes},Carta {tipusCarta = As, pal = Espases},Carta {tipusCarta = As, pal = Bastos},Carta {tipusCarta = Dos, pal = Oros},Carta {tipusCarta = Dos, pal = Copes},Carta {tipusCarta = Dos, pal = Espases},Carta {tipusCarta = Dos, pal = Bastos},Carta {tipusCarta = Tres, pal = Oros},Carta {tipusCarta = Tres, pal = Copes},Carta {tipusCarta = Tres, pal = Espases},Carta {tipusCarta = Tres, pal = Bastos},Carta {tipusCarta = Quatre, pal = Oros},Carta {tipusCarta = Quatre, pal = Copes},Carta {tipusCarta = Quatre, pal = Espases},Carta {tipusCarta = Quatre, pal = Bastos},Carta {tipusCarta = Cinc, pal = Oros},Carta {tipusCarta = Cinc, pal = Copes},Carta {tipusCarta = Cinc, pal = Espases},Carta {tipusCarta = Cinc, pal = Bastos},Carta {tipusCarta = Sis, pal = Oros},Carta {tipusCarta = Sis, pal = Copes},Carta {tipusCarta = Sis, pal = Espases},Carta {tipusCarta = Sis, pal = Bastos},Carta {tipusCarta = Set, pal = Oros},Carta {tipusCarta = Set, pal = Copes},Carta {tipusCarta = Set, pal = Espases},Carta {tipusCarta = Set, pal = Bastos},Carta {tipusCarta = Vuit, pal = Oros},Carta {tipusCarta = Vuit, pal = Copes},Carta {tipusCarta = Vuit, pal = Espases},Carta {tipusCarta = Vuit, pal = Bastos},Carta {tipusCarta = Manilla, pal = Oros},Carta {tipusCarta = Manilla, pal = Copes},Carta {tipusCarta = Manilla, pal = Espases},Carta {tipusCarta = Manilla, pal = Bastos},Carta {tipusCarta = Sota, pal = Oros},Carta {tipusCarta = Sota, pal = Copes},Carta {tipusCarta = Sota, pal = Espases},Carta {tipusCarta = Sota, pal = Bastos},Carta {tipusCarta = Cavall, pal = Oros},Carta {tipusCarta = Cavall, pal = Copes},Carta {tipusCarta = Cavall, pal = Espases},Carta {tipusCarta = Cavall, pal = Bastos},Carta {tipusCarta = Rei, pal = Oros},Carta {tipusCarta = Rei, pal = Copes},Carta {tipusCarta = Rei, pal = Espases},Carta {tipusCarta = Rei, pal = Bastos}]


a :: Carta
a = Carta As Oros

-- >>> tipusCarta a
-- As


-- >>> Carta As Oros > Carta Quatre Oros
-- No instance for (Ord Carta) arising from a use of ‘>’


-- >>> fst a
-- Couldn't match expected type ‘(a, b0)’ with actual type ‘Carta’

-- PRINCIPALS:
-- trampa:: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe ([Carta],Int, Int)
-- cartesGuanyades:: Trumfu -> [Carta] -> Int -> ([Carta],[Carta])
-- punts:: [Carta] -> Int
-- puntsParelles:: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe (Int, Int)

-- ADICIONALS:
-- cartesPal
-- palGuanyadorBasa
-- quiGuanya
-- quiSortira
-- jugades
-- basaCorrecta
