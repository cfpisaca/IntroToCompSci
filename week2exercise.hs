-- Carlo Pisacane cfpisaca@syr.edu

commonDivisor :: Integer -> Integer -> Integer -> Bool
commonDivisor q n m = (mod n q) == 0 && (mod m q) == 0

nand :: Bool -> Bool -> Bool
nand e1 e2 = not ((e1 == True) && (e2 == True))
     
distance :: Float -> Float -> Float -> Float -> Float
distance a b c d = sqrt((a - c)^2 + (b - d)^2)

