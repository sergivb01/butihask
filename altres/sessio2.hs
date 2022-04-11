data Arbre a = Buit | Node (Arbre a) a (Arbre a) 

-- Exercici adiccional 1
mapArbre :: (a -> b) -> Arbre a -> Arbre b
mapArbre f Buit = Buit
mapArbre f (Node e m d) = Node (mapArbre f e) (f m) (mapArbre f d)

-- Exercici adiccional 2
podaArbre :: (a -> Bool) -> Arbre a -> Arbre a
podaArbre f Buit = Buit
podaArbre f (Node e m d)
 | f m = Node (podaArbre f e) m (podaArbre f d)
 | otherwise = Buit

 -- Exercici adiccional 3