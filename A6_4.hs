import Distribution.Simple.Flag (Flag)
-- Datentyp für rationale Zahlen repräsentiert durch zwei Ganzzahlen für Zähler und Nenner
data Ratio = MkRatio Integer Integer


-- Gleichheit für rationale Zahlen
instance Eq Ratio where
  (MkRatio nom1 den1) == (MkRatio nom2 den2) = (nom1 * den2 == nom2 * den1)

-- Ordnung für rationale Zahlen
instance Ord Ratio where
  (MkRatio nom1 den1) <= (MkRatio nom2 den2) =
    if ((signum den1) * (signum den2) == 1)
      then (nom1 * den2 <= nom2 * den1)
      else (nom2 * den1 <= nom1 * den2)

-- Rechenoperationen für rationale Zahlen inklusive Addition, Multiplikation, Negierung, Betragsfunktion, Vorzeichenfunktion und Konvertierung von Integer
instance Num Ratio where
  (+) (MkRatio nom1 den1) (MkRatio nom2 den2) = (MkRatio (nom1 * den2 + nom2 * den1) (den1 * den2))
  (*) (MkRatio nom1 den1) (MkRatio nom2 den2) = (MkRatio (nom1 * nom2) (den1 * den2))
  negate (MkRatio nom den) = (MkRatio (- nom) den)
  abs (MkRatio nom den) = (MkRatio (abs nom) (abs den))
  signum (MkRatio nom den) = (MkRatio ((signum nom) * (signum den)) 1)
  fromInteger i = (MkRatio i 1)
  


-- Funktion zum Anzeigen von rationalen Zahlen
-- Die Notation wird später in der Vorlesung erläutert
instance Show Ratio where
  show (MkRatio nom den) = (show nom) ++ "/" ++ (show den)

ggT :: Integer -> Integer -> Integer
ggT a b = if b == 0 then a
    else ggT b (mod a b)

myRatio :: Ratio
myRatio = MkRatio 2 3

reduceFraction :: Ratio -> Ratio
reduceFraction (MkRatio nom den) = MkRatio (div nom (ggT nom den)) (div den (ggT nom den))

fromRatio :: Ratio -> Float
fromRatio (MkRatio nom den) = roundFive (fromIntegral nom / fromIntegral den)

roundFive :: Float -> Float
roundFive f = fromIntegral(round (f*10^5))/(10^5)

decimalPlace :: Float -> Integer
decimalPlace f = truncate (roundFive f*10^5)

toRatio :: Float -> Ratio
toRatio f = reduceFraction (MkRatio (decimalPlace f) 100000)

