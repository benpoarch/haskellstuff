module Q4 where

-- Question 4
-- a)
normaliseSpace:: String -> String
normaliseSpace "" = ""
{- (line 9) As there may be two or more space this line allows normaliseSpace 
to be called again continuously until all spaces are 1 -}
normaliseSpace (' ' : ' ' : xs) = normaliseSpace (' ':xs)
normaliseSpace (' ':xs) = ' ':normaliseSpace xs
-- (line 11) allows normliseSpace to be called on the rest of the string after x.
normaliseSpace (x:xs) = x : normaliseSpace xs
 
-- b)
normaliseFront :: String -> String
normaliseFront "" = ""
normaliseFront " " = ""
{- (line 20) if there is a space found at the beginning this line will just output
 the string minus the space. -}
normaliseFront (' ':x:xs) = normaliseFront(x:xs)
normaliseFront (x:xs) = x:xs

normaliseBack :: String -> String
normaliseBack "" = ""
{- (line 27) Reverse the string and utilise normaliseFront to remove any spaces from
 the front, and then reverse it again so the tail is back to being on the end.-}
normaliseBack (x:xs) = reverse(normaliseFront (reverse (x:xs)))
		  
-- c)
normalise:: String -> String
normalise "" = ""
-- (line 33) Combination of the above three functions used on the string.
normalise (x:xs) = normaliseSpace(normaliseFront(normaliseBack(x:xs)))


-- d)
prefix :: String -> String -> Bool
{-(line 40/41) first line returns false if second string is empty, and second returns 
true if first list is empty, because it will always be a substring of ys. -}
prefix (x:xs) "" = False
prefix "" ys = True
--(line 43) compares the first letter of each string and recursively repeats this
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

substr :: String -> String -> Bool

substr (x:xs) "" = False
{-(lines 48-54) checks if xs is a prefix of ys, and if not it recurses and checks if 
it is a prefix of the next letter of the string. This repeats. -}
substr xs ys = if prefix xs ys 
                  then True
                  else if substr xs (tail ys) 
				       then True
                       else False

postfix :: String -> String -> Bool
{- (line 59) reverses both strings and then checks if one is the prefix of the other,
 as this is the same as checking the postfix -}
postfix xs ys = prefix (reverse xs) (reverse ys)

-- e) 
substitute :: String -> String -> String -> String
substitute "" ys zs = zs
substitute xs ys "" = "" 
{- (line 67 -) resursively searches through zs for xs, and replaces it with ys when found,
 by dropping and then appending.-}
substitute xs ys (z:zs) = if prefix xs (z:zs) 
                          then ys ++ (substitute xs ys (drop (length xs) (z:zs)))
                          else [z] ++ substitute xs ys zs





