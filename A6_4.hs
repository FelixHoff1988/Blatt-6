import Distribution.Simple.Flag (Flag)
-- Funktion zum Anzeigen von rationalen Zahlen
-- Die Notation wird später in der Vorlesung erläutert
instance Show Ratio where
  show (MkRatio nom den) = (show nom) ++ "/" ++ (show den)
-- Eine Hilfsfunktion die den gößten gemeinsamen teiler zweier Zahlen a und b bestimmt indem sie a zurückgibt falls b = 0 ist. Ansonsten soll der größte gemeinsame Teiler von a Modulo b bestimmt werden.
--Beispiel ggT 4 6 sollte 2 ausgeben.
ggT :: Integer -> Integer -> Integer
ggT a b = if b == 0 then a
    else ggT b (mod a b)
-- Funktionen erstellen  Rationale Zahlen
myRatio :: Ratio
myRatio = MkRatio 2 3

myRatio2 :: Ratio
myRatio2 = MkRatio 4 6
-- Funktion die eine rationale Zahl vollständig kürzt. 
--Beispiel reduceFraction MyRatio2 sollte 2 / 3 ausgeben.
reduceFraction :: Ratio -> Ratio
reduceFraction (MkRatio nom den) = MkRatio (div nom (ggT nom den)) (div den (ggT nom den))

fromRatio :: Ratio -> Float
fromRatio (MkRatio nom den) = roundFive (fromIntegral nom / fromIntegral den)
-- Funktion die eine Gleitkommazahl auf fünf nachkommastellen rundet
-- Beispiel roundFive 2.1234567 gibt 2.12346 aus.
roundFive :: Float -> Float
roundFive f = fromIntegral(round (f*10^5))/(10^5)
-- Funktion die eine Gleitkommazahl die auf die fünfte Stelle gerundet wird  in eine Ganzzahl umwandelt, mit der besonderheit das die Zahlen nach dem Komma nun zur ganzen Zahl gehören. 
-- Beispiel decimalPlace 2.1234567 gibt 212346 aus.
decimalPlace :: Float -> Integer
decimalPlace f = truncate (roundFive f*10^5)
-- Funktion die eine Gleitkommazahl in eine Rationale Zahl umwandelt mit der Besonderheit das die Gleitkommazahl vor der umwandlung auf die 5. nachkommastelle gerundet wird.
-- Beispiel toRatio 2.1234567 sollte 106173 / 50000 ausgeben.
toRatio :: Float -> Ratio
toRatio f = reduceFraction (MkRatio (decimalPlace f) 100000)
