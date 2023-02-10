----------------------------------------------------------------------
--   CIS 252: Lab 7 (Spring 2022)
--
--       Name: Carlo Pisacane
--       Email: cfpisaca@syr.edu
----------------------------------------------------------------------
import Data.Char

----------------------------------------------------------------------
-- Task One:
--
-- For each of the following, uncomment the line and replace the
--   underscores/blanks by your answers.  (In some cases, the 
--   second blank can be deleted completely.)
----------------------------------------------------------------------

-- Generate the list: 
--    [1,2,3,4,5,6,7,8]

one = [ e-2 | e <- [3 .. 10] ]



-- Generate the list: 
--    [False,True,False,True,False,True,False,True]

two = [ even e | e <- [3 .. 10] ]



-- Generate the list: 
--    [[0,1,5],[0,1,6],[0,1,7],[0,1,8],[0,1,9]]

three = [ 0:1:[e] | e <- [3 .. 10], e > 4, e < 10 ]



-- Generate the list: 
--    [(6,False),(10,False),(14,False),(18,True)]

four = [ (e*2, mod e 9 == 0) | e <- [3 .. 10], odd e ]



-- Generate the list: 
--    [20,19,18,17,16,15,14,13]

five = [ 23-e | e <- [3 .. 10] ]



-- Generate the list: 
--    [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12)]

six = [ (2*e-5,2*e-4) | e <- [3 .. 10], e <= 8 ]




----------------------------------------------------------------------
-- Task Two:
--
-- For each of the following functions, uncomment the definitions
--   and replace the underscores/blanks with your code.  Don't
--   change anything else.
----------------------------------------------------------------------

addToEvens :: Integer -> [Integer] -> [Integer]
addToEvens j nums = [ j + x | x <- nums, odd x ]


letters :: String -> String
letters cs = [ toUpper x | x <- cs, isAlpha x ]







