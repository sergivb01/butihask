data Nat = Cero | Suc Nat deriving Show

instance Eq Nat where
    Cero == Cero = True
    Suc x == Suc y = x == y
    _ == _ = False

instance Ord Nat where
    Cero <= _ = True
    Suc x <= Suc y = x <= y
    _ <= _ = False

instance Num Nat where
    Cero + Cero = Cero
    Cero + Suc x = Suc x
    Suc x + Cero = Suc x
    Suc x + Suc y = Suc (Suc x) + y
    Cero * Cero = Cero
    Cero * _ = Cero
    _ * Cero = Cero
    Suc x * Suc y = Suc x + (Suc x * y)
    Cero - Cero = Cero
    Cero - _ = Cero
    x - Cero = x
    Suc x - Suc y = x - y
    abs Cero = Cero
    abs x = x
    fromInteger 0 = Cero
    fromInteger x = Suc (fromInteger (x-1))
    signum _ = Suc Cero

instance Enum Nat where
    toEnum a = Suc (toEnum (a-1))
    fromEnum Cero = 0
    fromEnum (Suc x) = 1 + fromEnum x

instance Real Nat

instance Integral Nat where

-- >>> 3 - Suc(Cero)
-- Suc (Suc Cero)
-- 
