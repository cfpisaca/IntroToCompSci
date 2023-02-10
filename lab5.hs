-- Carlo Pisacane cfpisaca@syr.edu

rotate3 :: Integer -> Char -> Char -> Char -> String
rotate3 n item1 item2 item3
        | n == 0    = ""
        | n > 0     = item1 : rotate3 (n-1) item2 item3 item1
        | otherwise = error "rotate3: requires nonnegative input"

descend :: Integer -> Char -> Char -> [String]
descend n item1 item2
        | n == 0    = []
        | n > 0     = rotate3 n item1 item1 item1 : descend (n-1) item2 item1
        | otherwise = error "descend: requires nonnegative input"




-- EXAMPLES FROM LAB

listOfZero :: Float -> [Float]
listOfZero item = []

listOfOne :: Float -> [Float]
listOfOne item = item : listOfZero item

listOfTwo :: Float -> [Float]
listOfTwo item = item : listOfOne item

listOfThree :: Float -> [Float]
listOfThree item = item : listOfTwo item

listOfFour :: Float -> [Float]
listOfFour item = item : listOfThree item

listOf :: Integer -> Float -> [Float]
listOf n item
     | n == 0    = []
     | n > 0     = item : listOf (n-1) item
     | otherwise = error "listOf: requires nonnegative input"

altListOf :: Integer -> Float -> Float -> [Float]
altListOf n item1 item2
          | n == 0    = []
          | n > 0     = item1 : altListOf (n-1) item2 item1
          | otherwise = error "altListOf: requires nonnegative input"


