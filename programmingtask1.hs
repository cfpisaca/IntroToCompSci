-- Carlo Pisacane cfpisaca@syr.edu


-- SOURCES 
-- This work was done all by myself and I have consulted no soruces other than slides and other class materials 


----- 1 -----
-- PURPOSE
-- To determine whether input (x1,y1) is on the line named ax + by + c = 0.

-- DEFINITION 
onLine :: (Float, Float) -> Float -> Float -> Float -> Bool
onLine (x1,y1) a b c 
       | a*(x1) + b*(y1) + c == 0 = True
       | otherwise = False
         
-- TESTS
-- onLine (2, (-2)) 0 0 0 = True
-- Test degenerate line with c equal to zero, b/c then every point satisfies the equation
-- onLine (9, 25) 0 0 3 = False
-- Test degenerate line with c not equal to zero, b/c then no point should satisfy the equation
-- onLine (10, (-4)) 2 6 4 = True
-- Test equation with a point that fits
-- onLine (1, (-1)) 1 0 2 = False
-- Test equation with a point that doesn't fit


----- 2 ------
-- PURPOSE
-- To determine whether the equation ax + by + c = 0 is degenerate. 

-- DEFINITION
degenerate :: Float -> Float -> Float -> Bool
degenerate a b c
           | a == 0 && b == 0 = True
           | otherwise = False 

-- TESTS
-- degenerate 0 0 0 = True
-- Fully degenerate equation test
-- degenerate 0 0 5 = True
-- Degenerate equation with a value of c other than 0
-- degenerate 1 0 5 = False
-- degenerate 0 1 5 = False
-- degenerate 1 1 5 = False
-- Testing nondegenerate equations in the previous three tests


----- 3 ------
-- PURPOSE
-- To determine whether the equation ax + by + c = 0 is a horizonal line. (Here you may assume that the equation is not degenerate.)

-- DEFINITION
horizontal :: Float -> Float -> Float -> Bool
horizontal a b c 
           | a == 0 = True
           | otherwise = False
      
-- TESTS
-- horizontal 8 2 3 = False
-- Testing with non horizontal input
-- horizontal 3 0 1 = False
-- Testing with vertical input
-- horizontal 0 3 1 = True
-- Testing with horizontal input
-- No need to test for degenerate lines


----- 4 ------
-- PURPOSE
-- To determine whether the equation ax + by + c = 0 is a vertical line. (Here you may assumethat the equation is not degenerate.)

-- DEFINITION
vertical :: Float -> Float -> Float -> Bool
vertical a b c
         | b == 0 = True
         | otherwise = False

-- TESTS
-- vertical 3 4 1 = False
-- Testing with non vertical input
-- vertical 9 2 3 = False
-- Testing with horizontal input
-- vertical 3 0 1 = True
-- Testing with vertical input
-- No need to test for degenerate lines


----- 5 ------
-- PURPOSE
-- Returns the x-coord of the x-intercept of the line ax + by + c = 0, when not degenerate and not horizontal. If degenerate or horizontal, 0.0 is returned.

-- DEFINITION
xIntercept :: Float -> Float -> Float -> Float
xIntercept a b c
           | degenerate a b c || horizontal a b c || c == 0 = 0.0
           | otherwise = -c/a

-- TESTS
-- xIntercept 0 0 0 = 0.0
-- Testing with degenerate input
-- xIntercept 0 4 9 = 0.0
-- Testing with horizontal input
-- XIntercept 9 0 5 = -0.5555556
-- Testing with vertical input
-- xIntercept 4 5 0 = 0.0
-- Testing with zero c input
-- xIntercept 9 3 13 = -1.444444
-- Testing with nondegenerate, non horizontal, non vertical, and non zero c input


----- 6 ------
-- PURPOSE
-- Returns the y-coord of the y-intercept of the line ax + by + c = 0, when not degenerate and not vertical. If degenerate or vertical, 0.0 is returned.

-- DEFINITION
yIntercept :: Float -> Float -> Float -> Float
yIntercept a b c
           | degenerate a b c || vertical a b c || c == 0 = 0.0
           | otherwise = -c/b

-- TESTS
-- yIntercept 0 0 0 = 0.0
-- Testing with degenerate input
-- yIntercept 4 0 9 = 0.0
-- Testing vertical input
-- yIntercept 0 9 5 = -0.5555556
-- Testing horizontal input
-- yIntercept 4 5 0 = 0.0
-- Testing with zero c input
-- yIntercept 9 3 13 = -4.3333335
-- Testing with nondegenerate, non vertical, non horizontal, and non zero c input


----- 7 ------
-- PURPOSE
-- Tests whether two lines a1x + b1x + c1 = 0 and a2x + b2x + c2 = 0 are parallel (Assuming both lines are nondegenerate). 

-- DEFINITION
parallel :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
parallel a1 b1 c1 a2 b2 c2
         | a1*b2 == a2*b1 = True
         | otherwise = False 
         
-- TESTS
-- parallel 3 9 3 3 9 2 = True
-- Testing parallel equations with same slopes
-- parallel 3 9 2 4 1 9 = False
-- Testing non parallel equations with different slopes
-- parallel 3 9 2 3 6 1 = False
-- Testing non parallel equations once again but with the same a value
-- parallel 0 2 2 0 9 4 = True
-- Testing horizontal equations that are parallel
-- parallel 2 0 2 9 0 4 = True
-- Testing vertical equations that are parallel


----- 8 ------
-- PURPOSE
-- Tests whether two lines a1x + b1x + c1 = 0 and a2x + b2x + c2 = 0 intersect (If either line is degenerate False is returned).

-- DEFINITION
intersect :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
intersect a1 b1 c1 a2 b2 c2
          | degenerate a1 b1 c1 || degenerate a2 b2 c2 = False
          | parallel a1 b1 c1 a2 b2 c2 = False
          | otherwise = True

-- TESTS
-- intersect 0 0 0 0 0 0 = False
-- Testing fully degenerate equation
-- intersect 0 0 9 0 0 4 = False
-- Testing degenerate equation with c values other than 0
-- intersect 3 9 3 3 9 2 = False
-- Testing parallel equations with same slopes
-- intersect 0 2 2 0 9 4 = False
-- Testing horizontal equations that are parallel
-- intersect 2 0 2 9 0 4 = False
-- Testing vertical equations that are parallel
-- intersect 9 18 2 8 7 2 = True
-- Testing regular intersecting equation


----- 9 ------
-- PURPOSE
-- Returns x-y-coords of two lines a1x + b1x + c1 = 0 and a2x + b2x + c2 = 0, when they intersect. If they don't intersect, (0.0, 0.0) is returned.

-- DEFINITION
intersectionPt :: Float -> Float -> Float -> Float -> Float -> Float -> (Float, Float)
intersectionPt a1 b1 c1 a2 b2 c2
               | intersect a1 b1 c1 a2 b2 c2 = ((b1*c2)-(b2*c1)/(a1*b2)-(a2*b1),(a2*c1)-(a1*c2)/(a1*b2)-(a2*b1))
               | otherwise = (0.0,0.0)
               
           
-- TESTS
-- intersectionPt 2 0 2 9 0 4 = (0.0,0.0)
-- Testing lines that do not intersect
-- intersectionPt 2 3 1 4 2 1 = (-9.5,-8.5)
-- Testing lines that intersect
-- intersectionPt 2 (-9) 22 4 19 5 = (-20.0,123.73684)
-- Testing more lines that intersect


----- 10 ------
-- PURPOSE
-- Determines whether the lines a1x + b1x + c1 = 0 and a2x + b2x + c2 = 0 are the same line. If either line is degenerate, False is returned.

-- DEFINITION
lineEqual :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
lineEqual a1 b1 c1 a2 b2 c2
          | degenerate a1 b1 c1 || degenerate a2 b2 c2 = False
          | parallel a1 b1 c1 a2 b2 c2 == False = False
          | (xIntercept a1 b1 c1 == xIntercept a2 b2 c2) && (yIntercept a1 b1 c1 == yIntercept a2 b2 c2) = True
          | otherwise = False
      
          
-- TESTS
-- lineEqual 0 0 0 0 0 0 = False
-- Testing degenerate equation
-- lineEqual 5 10 6 (-3) 1 12 = False
-- Testing non parallel equation
-- lineEqual 10 2 3 25 5 (-4) = False
-- Testing parallel equation with different x-y-intercepts
-- lineEqual 3 5 12 6 10 24 = True
-- Testing parallel equations with same x-y-intercepts
-- lineEqual (-10) 2 (-1) 25 2 (-1)
-- Testing lines with same y-intercepts
-- lineEqual 25 2 (-1) 25 5 (-1)
-- Testing lines with same x-intercepts

