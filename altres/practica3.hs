-- PRÀCTICA 2

data Nat = Cero | Suc Nat deriving Show

-- Exercici 1
instance Eq Nat where
    Cero == Cero = True
    Suc x == Suc y = x == y
    _ == _ = False
-- >>> Cero == Cero
-- >>> Suc (Suc Cero) == Cero
-- >>> Cero /= Suc (Suc Cero)
-- >>> Suc Cero == Suc (Suc Cero)
-- >>> Suc (Suc Cero) == Suc (Suc Cero)
-- True
-- False
-- True
-- False
-- True

-- Exercici 2
instance Ord Nat where
    Cero <= _ = True
    Suc x <= Suc y = x <= y
    _ <= _ = False
-- >>> Cero < Cero
-- >>> Suc Cero <= Suc (Suc Cero)
-- >>> max (Suc Cero) (Suc (Suc Cero))
-- False
-- True
-- Suc (Suc Cero)

-- Exercici 3
instance Num Nat where
    Cero + x = x
    (Suc x) + y = Suc(x + y)
-- >>> Suc Cero + Suc (Suc Cero)
-- Suc (Suc (Suc Cero))

    Cero * _ = Cero
    (Suc x) * y = y + (x * y)
-- >>> Suc (Suc Cero) * Suc (Suc Cero)
-- Suc (Suc (Suc (Suc Cero)))

    fromInteger 0 = Cero
    fromInteger x = Suc (fromInteger (x - 1))

-- >>> fromInteger 3 + Suc Cero
-- Suc (Suc (Suc (Suc Cero)))
    
instance Enum Nat where
-- Exercici 4
    toEnum 0 = Cero
    toEnum x = toEnum(x - 1)

    fromEnum Cero = 0
    fromEnum (Suc x) = 1 + fromEnum x


-- >>> fromEnum (Suc (Suc Cero))
-- >>> pred (Suc Cero)
-- >>> succ (Suc (Suc Cero))
-- 2
-- Cero
-- Cero

-- >>> Nat::toEnum 6
-- Illegal type: ‘6’ Perhaps you intended to use DataKinds

