-- Name: Carlo Pisacane
-- Email: cfpisaca@syr.edu

-- SOURCES 
-- This work was done all by myself and I have consulted no soruces other than slides and other class materials 

type Bag a = [(a,Integer)]

charBag1 :: Bag Char
charBag1 = [('e',1),('y',2),('z',1)]

charBag2 :: Bag Char
charBag2 = [('e',4),('y',2),('z',3),('j',2)]

charBag3 :: Bag Char
charBag3 = [('y',3),('j',1)]

stringBag1 :: Bag String
stringBag1 = [("juice",2),("cereal",1),("toothpaste",3),("soup",1)]

nonBag :: [(String,Integer)]
nonBag = [("u",7),("x",(-1)),("x",3)]


-- RECURSIVE FUNCTIONS


----- 1 -----
-- PURPOSE
-- To calculate the total number of items in bag.

-- DEFINITION
totalItems :: Bag a -> Integer
totalItems [] = 0
totalItems ((c,n):r) = n + totalItems r
          
-- TESTS
-- Tests are self-explanatory, every test has same path.
-- totalItems [('x',2),('y',3),('z',1)] -> 6, adds up number of copies for each item
-- totalItems [('x',1)] -> 1, adds up number of copies for each item


----- 2 -----
-- PURPOSE
-- To calculate the number of copies of item in bag.

-- DEFINITION
howMany :: Eq a => a -> Bag a -> Integer
howMany item [] = 0 
howMany item ((x,y):xs)
        | x == item = y
        | otherwise = howMany item xs

-- TESTS 
-- howMany 'y' [('x',2),('y',9),('z',3)] -> 9, finds the correct item and returns amount of copies
-- howMany 'a' [('x',2),('y',9),('z',3)] -> 0, no item 'a' in bag so returns 0 


----- 3 -----
-- PURPOSE
-- To add one additional copy of a certain item in the bag.

-- DEFINITION

addToBag :: Eq a => a -> Bag a -> Bag a
addToBag item [] = [(item,1)]
addToBag item ((x,y):xs)
         | x == item = ((x,y+1):xs)
         | otherwise = (x,y) : addToBag item xs
         
-- TESTS (explain)
-- addToBag 'a' [('x',2),('a',1),('y',9),('z',3)] -> [('x',2),('a',2),('y',9),('z',3)], finds correct item and adds one copy to the item
-- addToBag '*' [('x',2),('a',1),('y',9),('z',3)] -> [('x',2),('a',1),('y',9),('z',3),('*',1)], no item '*' in bag so adds one copy to the end of the list


----- 4 -----
-- PURPOSE
-- To remove one copy of a certain item in the bag.

-- DEFINITION
removeFromBag :: Eq a => a -> Bag a -> Bag a
removeFromBag item [] = [] 
removeFromBag item ((x,y):xs)
         | item == x && y > 1 = (x,y-1) : removeFromBag item xs
         | item == x && y == 1 = removeFromBag item xs
         | otherwise = (x,y) : removeFromBag item xs

-- TESTS (explain)
-- removeFromBag 'a' [('x',3),('a',2),('z',1)] -> [('x',3),('a',1),('z',1)], finds the item and removes one copy from the item 
-- removeFromBag 'z' [('x',3),('a',2),('z',1)] -> [('x',3),('a',2)], finds the item and removes one copy, including the entire tuple b/c there was only one copy
-- removeFromBag 'z' [('x',3),('a',9)] -> [('x',3),('a',9)] -> no item 'z' in bag so returns the list


----- 5 -----
-- PURPOSE
-- To create a bag that contains all of the characters from the input.

-- DEFINITION
createBagFrom :: String -> Bag Char
createBagFrom [] = []
createBagFrom (x:xs) = addToBag x (createBagFrom xs)

-- TESTS (explain)
-- createBagFrom "hello!" -> [('!',1),('o',1),('l',2),('e',1),('h',1)], goes through each char in input and uses addToBag to add them to the final list
-- createBagFrom "wow cool" -> [('l',1),('o',3),('c',1),(' ',1)('w',2)], goes through each char in input and uses addToBag to add them to the final list


----- 6 -----
-- PURPOSE
-- To determine whether bag 1 is a subbag of bag 2.
 
-- Definition
subBag :: Eq a => Bag a -> Bag a -> Bool
subBag [] bag2 = True
subBag ((x,y):xs) bag2
       | howMany x bag2 >= y = subBag xs bag2
       | otherwise = False 

-- TESTS (explain)
-- subBag [('x',3),('y',2),('z',2)] [('x',4),('y',2),('z',7),('w',1)] -> True, bag2 contains all items in bag1 and the number of copies in bag2 are equal or over in bag2
-- subBag [('a',1),('i',1)] [('a',2),('b',4),('c',9),('d',1)] -> False, bag2 does not contain all items in bag1 so bag1 is not a sub bag of bag2


----- 7 -----
-- PURPOSE
-- To determine whether a bag is valid or not.

-- DEFINITION
validBag :: Eq a => Bag a -> Bool
validBag [] = True
validBag ((x,y):xs)
         | y <= 0 = False
         | (howMany x xs > 1) = False
         | otherwise = validBag xs


-- TESTS (explain)
-- validBag [('x',1),('y',2),('z',9)] -> True, no item has a number of copies below 1 and there are no duplicate items
-- validBag [('x',0),('y',1)] -> False, item 'x' has a number of copies below 1, so it is an invariant bag
-- validBag [('x',1),('x',3)] -> False, item 'x' has a duplicate item in the same bag, so it is an invariant bag

updateLabels :: Integer -> BTree Integer -> BTree Integer
updateLabels num Empty = Empty
updateLabels num (BNode r Empty Empty) = BNode (r-num) (updateLabels num left) (updateLabels num right)
updateLabels num (BNode r left right) = BNode (r+num) (updateLabels num left) (updateLabels num right)
              

data BTree a = Empty
             | BNode a (BTree a) (BTree a)
             deriving (Show)




