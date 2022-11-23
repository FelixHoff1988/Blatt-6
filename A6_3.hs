pascal :: Int -> Int -> Int
pascal n k 
    | k == 0 = 1
    | n == k = 1
    | otherwise = if k>n 
        then 0
        else
            pascal (n-1) (k-1) + pascal (n-1) k
