import Buti


---------------------------------------------------- PRINCIPALS ----------------------------------------------------


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

