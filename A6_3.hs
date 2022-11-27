-- Die Funktion nimmt n über k entgegen und gibt den Binomialkoeffizienten aus.
-- Beispiel  für n = 3 und k = 1 gibt 3 aus.
-- Diese Funktion ist eine direkte rekursive Funktion da sie (pascal) sich selbst wieder aufruft (pascal).
pascal :: Int -> Int -> Int
pascal n k 
    | k == 0 = 1
    | n == k = 1
    | otherwise = if k>n 
        then 0
        else
            pascal (n-1) (k-1) + pascal (n-1) k
