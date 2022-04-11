data ArbreB a = Buit | NodeB (ArbreB a) a (ArbreB a) deriving (Show)

mapArbre :: (a -> b) -> ArbreB a -> ArbreB b
mapArbre _ Buit = Buit
mapArbre fun (NodeB esq val dre) = NodeB (r esq) (fun val) (r dre) where r = mapArbre fun

a :: ArbreB Integer
a = NodeB a1 3 a2
  where
    a1 = NodeB Buit 5 Buit
    a2 = NodeB a3 7 a1
    a3 = NodeB a4 18 Buit
    a4 = NodeB Buit 48 Buit

-- >>> a
-- >>> mapArbre (*2) a
-- NodeB (NodeB Buit 5 Buit) 3 (NodeB (NodeB (NodeB Buit 48 Buit) 18 Buit) 7 (NodeB Buit 5 Buit))
-- NodeB (NodeB Buit 10 Buit) 6 (NodeB (NodeB (NodeB Buit 96 Buit) 36 Buit) 14 (NodeB Buit 10 Buit))

podaArbre :: (a -> Bool) -> ArbreB a -> ArbreB a
podaArbre _ Buit = Buit
podaArbre pred (NodeB esq val dre)
  | pred val = NodeB (r esq) val (r dre)
  | otherwise = Buit
  where
    r = podaArbre pred

-- >>> podaArbre (<48) a
-- NodeB (NodeB Buit 5 Buit) 3 (NodeB (NodeB Buit 18 Buit) 7 (NodeB Buit 5 Buit))

insereixOrdenat :: Integer -> ArbreB Integer -> ArbreB Integer
insereixOrdenat n Buit = NodeB Buit n Buit
insereixOrdenat n (NodeB esq val dre)
  | n < val = NodeB (r esq) val dre
  | n > val = NodeB esq val (r dre)
  | otherwise = NodeB esq val dre
  where
    r = insereixOrdenat n

b :: ArbreB Integer
b = NodeB a1 10 a2
  where
    a1 = NodeB a3 8 Buit
    a2 = NodeB a4 12 a5
    a3 = NodeB Buit 5 Buit
    a4 = NodeB Buit 11 Buit
    a5 = NodeB Buit 14 Buit

-- >>> b
-- >>> insereixOrdenat 1 b
-- >>> insereixOrdenat 10 b
-- NodeB (NodeB (NodeB Buit 5 Buit) 8 Buit) 10 (NodeB (NodeB Buit 11 Buit) 12 (NodeB Buit 14 Buit))
-- NodeB (NodeB (NodeB (NodeB Buit 1 Buit) 5 Buit) 8 Buit) 10 (NodeB (NodeB Buit 11 Buit) 12 (NodeB Buit 14 Buit))
-- NodeB (NodeB (NodeB Buit 5 Buit) 8 Buit) 10 (NodeB (NodeB Buit 11 Buit) 12 (NodeB Buit 14 Buit))


inordre :: ArbreB a -> [a]
inordre Buit = []
inordre (NodeB esq val dre) = inordre esq ++ [val] ++ inordre dre

-- >>> inordre a
-- >>> inordre b
-- [5,3,48,18,7,5]
-- [5,8,10,11,12,14]
