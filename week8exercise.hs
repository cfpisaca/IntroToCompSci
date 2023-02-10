spooky :: [(Char,Integer,Float)] -> [Bool]
spooky ts = [ m > 30 | (a,m,e) <- ts, isLower a]

spooky' :: [(Char,Integer,Float)] -> [Bool]
spooky' [] = []
spooky' ((a,m,e):as)
      | isLower a = (m > 30) : spooky as
      | otherwise = spooky as 

trick :: [(Integer,Char)] -> [Char]
trick [] = []
trick ((e,k):es)
      | even e = toUpper k : trick es
      | otherwise = trick es 

trick' :: [(Integer,Char)] -> [Char]
trick' ps = [ toUpper k | (e,k) <- ps, even e ]



cheer :: [(Integer,Integer)] -> [Bool]
cheer ps = [ even (e+b) | (e,b) <- ps, b > 5*e ]

cheer2 :: [(Integer,Integer)] -> [Bool]
cheer2 [] = []
cheer2 ((e,b):es)
       | b > 5*e = (even (e+b)) : cheer2 es
       | otherwise = cheer2 es
       
rally :: Int -> [String] -> [[String]]
rally k [] = []
rally k (w:gs)
      | length w < k = [w,w] : rally k gs
      | otherwise = rally k gs


rally' :: Int -> [String] -> [[String]]
rally' k strs = [ [w,w] | w <- strs, length w < k ]
