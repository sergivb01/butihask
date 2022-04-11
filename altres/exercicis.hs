data Nat = Cero | Suc Nat deriving (Show)

-- Exercici 3.1: fer que el tipus Nat sigui instancia de classe Eq
instance Eq Nat where
  Cero == Cero = True
  Suc x == Suc y = x == y
  _ == _ = False

-- >>> Cero == Cero -- True
-- >>> Suc (Suc Cero) == Cero -- False
-- >>> Cero /= Suc (Suc Cero) -- True
-- >>> Suc Cero == Suc (Suc Cero) -- False
-- True
-- False
-- True
-- False

-- Exercici 3.2: fer que el tipus Nat sigui instancia de classe Ord
instance Ord Nat where
  Cero <= _ = True
  Suc x <= Suc y = x <= y
  _ <= _ = False

-- >>> Cero < Cero -- False
-- >>> Suc Cero <= Suc (Suc Cero) -- True
-- >>> max (Suc Cero) (Suc (Suc Cero)) -- Suc (Suc Cero)
-- False
-- True
-- Suc (Suc Cero)

-- Exercici 3.3: fer que el tipus Nat sigui instancia de classe Num
instance Num Nat where
  Cero + x = x
  (Suc x) + y = Suc (x + y) -- O  sino tambe: x + Suc y

  Cero * _ = Cero
  (Suc x) * y = y + (x * y)

  fromInteger 0 = Cero
  fromInteger x = Suc (fromInteger (x - 1))
  abs x = x
  negate = error "no es pot negar"
  signum = error "signum"

-- >>> Cero + Cero
-- Cero

-- >>> Suc (Suc Cero) * Suc (Suc Cero)
-- Suc (Suc (Suc (Suc Cero)))

-- >>> fromInteger 3 + Suc Cero
-- fromInteger

instance Enum Nat where
  toEnum 0 = Cero
  toEnum n = Suc (toEnum (n - 1))
  fromEnum Cero = 0
  fromEnum (Suc x) = 1 + fromEnum x

-- >>> toEnum 5
-- >>> Suc Cero + toEnum 4
-- Illegal type: ‘5’ Perhaps you intended to use DataKinds

-- >>> pred (Suc Cero)
-- >>> fromEnum (Suc Cero)
-- Cero
-- 1
