contrived :: Int -> Int -> Int
contrived m n
          | even n && m > n = n * 2
          | odd n || n < 3  = m
          | otherwise       = m + n + 1


