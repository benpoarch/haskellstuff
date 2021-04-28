import Test.QuickCheck
import Data.Char

--Question 1
-- a) 
{- max1 checks if x-y, x-z and y-z (respectively depending on circumstance)
 are equal to or greater than 0 in order to find the largest of the 3 numbers.-} 
max1 :: Int -> Int -> Int -> Int 
max1 x y z = if x - y >= 0
             then if x - z >= 0
			      then x
				  else z
			 else if y - z >= 0
			      then y
				  else z
				  
{- max2 checks if x is greater than or equal to y and if the bigger of x/y is bigger
 than z, in order to discover the largest of the 3 numbers.-}
max2 :: Int -> Int -> Int -> Int
max2 x y z = if x >= y
             then if x >= z
			      then x
				  else z
		     else if y >= z
			      then y
				  else z
-- (query(ies) max1 978316 979610 986963 / max2 978316 979610 986963
-- (answer)    986963

-- b)				  
checkcorrectness x y z = max1 x y z == max2 x y z
-- haskell response = +++ OK, passed 100 tests.

{- Additional question - Possible weakness of correctness check 
 Answer: Potential weakness is that even though a failure would show one of the 
 functions doesn't work correctly, it doesn't identify which one. which also
 means there is potential that the test could pass as both functions may be faulty 
 in the same way. -}

--Question 2
{- In this luigi function there are calculations to work out the area of a circle. 
In order to round up to two decimal places the round function is used on the sum 
of x + y multiplied by 100, once rounded (to an int) it is then divided by 100 in
order to obtain the 2 dp numbers. fromInteger allows the int to be taken and 
represented as a decimal.-}

luigi :: Float -> Float -> Float
luigi  x y = (fromInteger $ round $ (((((x*0.6) +((((y/2)^2) * pi)*0.002))*1.6)*100))) / 100

{- Answer to question, which is more expensive
Pizza Bambini = 6 toppings and 15cm pizza = £6.33
Pizza Famiglia = 2 toppings and 32 cm pizza = £4.49 
Bambini is the more expensive pizza -} 

-- Question 3
--a) List Comprehension
counta :: [Char] -> Int
counta [] = 0				   
counta x = let xs = [xs| xs <-x, isDigit xs]
               in length xs
			   
--b) Higher Order Function
countb :: [Char] -> Int
countb [] = 0
countb x = length (filter(isDigit) x) 


-- c) Recursion
countc :: [Char] -> Int
countc [] = 0
countc (x:xs) = if isDigit x
                then 1 + countc xs
				else countc xs
				
-- Question 5
-- a) 
won :: [Int] -> Bool
won = all(==0)
