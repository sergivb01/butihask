import Buti


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

a = [Carta As Bastos, Carta As Oros,Carta As Espases]
-- >>> a !! posicioLlista a (Carta As Espases)
-- >>> posicioLlista a (Carta As Espases)
-- As de Espases
-- 2

-- >>> quiGuanya [Carta Vuit Bastos, Carta As Oros, Carta Manilla Oros, Carta Cavall Bastos] Butifarra
-- (Cavall de Bastos,3)

-- >>> length [Carta Vuit Bastos, Carta As Oros, Carta Manilla Oros]
-- 3

