--Vorlesungsbeispiel für den ADT A
--Signatur:
class ADT a where
    a, b :: a
    op1   :: a -> a
    op2, op3  :: a -> a -> a
--Semantik
    op1 a = b
    op2 a x = x
    op3 b x = x

--Beispiel 1: ADT Color
--Signatur:
class Color c where
    red, green, blue, yellow    :: c
    kompleColor                 :: c -> c
    mixColor                    :: c -> Float -> c
    lightenColor                :: c -> c -> c
--Semantik
    kompleColor red = green
    mixColor blue yelllow = green
    lightenColor c f = c

--Beispiel 2: ADT Fish
--Signatur:
class Fish f where
    shark, trout :: f
    saltwaterFish   :: f -> Bool
    isLarger, isStronger  :: f -> f -> f
--Semantik
    saltwaterFish f = True
    isLarger f g = f
    isStronger f g = g