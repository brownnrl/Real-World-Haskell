import Prelude hiding (Left, Right)

import Data.List hiding (intersperse) 

--- Real World Haskell Problems worked, Chapter 3

--- {{{ All Problems

---{{{ List Problems #1.1, 2.1, 2.2

---{{{ List type (note: isomorphic to [], because we can write a fromList and toList functions forming a bijection)

data List a = Cons a (List a)
            | Nil
            deriving (Show)

---}}}

---{{{ #1.1 : fromList function and toList function

fromList :: List a -> [a]
fromList Nil          = []
fromList (Cons a b)   = a:(fromList b)

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = (Cons x (toList xs))

---}}}

---{{{ #2.1, 2.2 : Compute the number of elements in a [x]


-- This is how I first implemented it.  There are a number of other implementations in the comments of the web page.
-- On this machine, I get a stack overflow on naiveRecursiveLength [1..1000000]
naiveRecursiveLength::[a]->Int
naiveRecursiveLength []  = 0
naiveRecursiveLength (x:xs) = 1 + (naiveRecursiveLength xs)

-- Here the h function is a guard to "force" the computation of acc+1 on each call, which is avoided
-- by haskell's lazy evaluation otherwise.
-- Takes a noticeable delay, but I get 1000000 on rascehpkinLength [1..1000000] (note: this machine is pretty slow)
raschepkinLength :: [a] -> Int
raschepkinLength xs = h 0 xs
                      where h acc (x:xs) | acc>=0    = h (acc+1) xs
                            h acc [] = acc 

-- The same as raschepkin's length function, avoiding a guard expression.
-- Operates the same for the most part as raschepkinLength
blanshardLength :: [a] -> Int
blanshardLength xs  = len xs 0
                      where len [] count = count
                            len (x:xs) count = len xs $! count + 1

-- There are a couple of additional examples in the comments of that problem, which involving unrolling the list
-- into a set of base cases, but there is no evidence that this works any better or worse than the naive/two other
-- functions listed in this section of code.

---}}}

---{{{ #2.3 Compute the mean of a list

mean :: (Fractional a) => [a] -> a
mean xs = (sum xs) / (fromIntegral (length xs))

---}}}

---{{{ #2.4, 2.5 Calculate the palidrome of a list.
isPalidrome :: (Eq a) => [a] -> Bool
isPalidrome xs | xs == (reverse xs) = True
               | otherwise          = False

toPalidrome :: (Eq a) => [a] -> [a]
toPalidrome xs | isPalidrome xs = xs
               | otherwise      = concat [xs,(reverse xs)]

---}}}

---{{{ #2.6 Sort a list of lists based on the list length using the sortBy function

-- The sortBy function has type (a -> a -> Ordering) -> [a] -> [a]
-- So it takes a function which compares two instances of a type a and returns an Ordering
-- an Ordering is defined as "data Ordering = LT | EQ | GT"

-- | Takes two lists and returns an Ordering by length of the list.
orderingByLength ::  [a] -> [a] -> Ordering
orderingByLength xs ys | lxs > lys = GT
                       | lxs < lys = LT
                       | otherwise = EQ
                       where lxs = length xs
                             lys = length ys

-- | Sorts a list of lists by the length of the lists. Fewer elements before more elements.
sortByLength :: [[a]] -> [[a]]
sortByLength xss = sortBy orderingByLength xss

---}}}

---{{{ #2.7 Define a function that joins a list of lists with a separator value.


-- |Joins together a list of lists using a separator value.
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (x:[]) = x -- You need this to avoid the last element being delimited.
intersperse delim (x1:xs) = x1 ++ [delim] ++ (intersperse delim xs) 

-- Note the above implementation has a behavior to include delimiters around empty elements
-- a suggest additional pattern match in the definition of this function by Atif Faridi:
-- intersperse delim ([]:xs) = intersperse delim xs
-- The above will ignore empty lists.

-- The following mildeIntersperse function was intersting.  It used a fairly 
-- compact notation and the foldl1 function.
--
-- foldl1 :: (a->a->a) -> [a] -> ai
-- it takes the first 2 items of the list and applies the function to them,
-- then feeds the function with this result and the third argument and so on. 
-- See scanl1 for intermediate results
--
-- I had never seen the inner function of the composition: (++ [delim])
--
-- This is known as a "section", see http://www.haskell.org/tutorial/functions.html
--
-- Infix operators can be applied partially, so (++ [delim]) is a function \y -> y ++ [delim]
--
-- If this was ([delim] ++) then \y -> [delim] ++ y
--
-- Finally, ((++) x y) = x ++ y 
--
-- [this I had known from a reverse polish calculators, 
-- but I was unaware of the above inflix representations]
-- 
-- So working through the express (foldl1 $ (++).(++ [delim])) xs
--
-- The inner function takes a list, say ys, and maps it to ys ++ [delim].
--
-- This list (ys ++ [delim]) is taken as the first argument of (++).
--
-- So we have a function (++) (ys ++ [delim]) zs, where zs is the second list argument.
--
-- This will result in: ys ++ [delim] ++ zs
--
-- So as the foldl1 function describes above, ys and zs will be the first two elements in the list
-- and then that result will be the first argument and the next element will be the next argument. 
--
-- mildeIntersperse :: a -> ([[a]]->[a])
mildeIntersperse delim [] = []
mildeIntersperse delim xs = (foldl1 $ (++).(++ [delim])) xs
---}}}

---{{{ Some functions exploring finding items in a list.

-- I had coded the below in a couple of attempts to do the convex hull problem.
-- Afterwards, I determined it was not necessary.  There is something similar to
-- this (almost exactly the same idea, actually) at
-- http://learnyouahaskell.com/recursion

-- | Finds an item in a list that is guarenteed to exist
-- | The compareItems should return if the first argument compares
-- | favorably when compared to the second argument (returns true)
-- | otherwise it returns false. See findMinInList and findMaxInList.
findInList2 :: (a -> a -> Bool) -> [a] -> a
findInList2 compareItems ([x1]) = x1
findInList2 compareItems (x1:xs) | (compareItems x1 cxs) = x1
                                 | otherwise             = cxs
                                 where cxs = findInList2 compareItems xs  


-- | Finds an item in a list that is guarenteed to exist
-- | The compareItems should return if the first argument compares
-- | favorably when compared to the second argument (returns true)
-- | otherwise it returns false. See findMinInList and findMaxInList.
-- | foldl1' uses strict evaluation.
findInList :: (a->a->a) -> [a] -> a
findInList compareItems = foldl1' compareItems


-- | Find the minimum element in a list.
findMinInList :: (Ord a) => [a] -> a
findMinInList = findInList min

-- | Find the maximum element in a list.
findMaxInList :: (Ord a) => [a] -> a
findMaxInList = findInList max

---}}}

---}}}

--- {{{ BinaryTree Problems #1.2, #2.8

--- {{{ Node and BinaryTree types #1.2
data Node a = Node a deriving (Show) 

data BinaryTree a = BinaryTree { node       :: Node a,
                                 leftTree   :: Maybe (BinaryTree a),
                                 rightTree  :: Maybe (BinaryTree a)
                               } deriving (Show)
--- }}}

--- {{{ BinaryTree Constructors (binaryTree and binaryLeaf)
binaryTree :: a -> Maybe (BinaryTree a) -> Maybe (BinaryTree a) -> BinaryTree a
binaryTree a b c = BinaryTree { node = Node a, leftTree=b, rightTree=c }

binaryLeaf :: a-> BinaryTree a
binaryLeaf a = binaryTree a Nothing Nothing
---}}}

---{{{ Helper functions fromNode and fromTree
fromNode :: Node a -> a
fromNode (Node a) = a

fromTree :: BinaryTree a -> a
fromTree t = fromNode (node t)
---}}}

---{{{ binaryTreeHeight function #2.8
-- binaryTreeHeight :: (Eq a) => BinaryTree a -> Int
binaryTreeHeight (BinaryTree { node=_, leftTree = Nothing, rightTree = Nothing } ) = 1 
binaryTreeHeight (BinaryTree { node=_, leftTree = Nothing, rightTree = Just rt } ) = 1 + binaryTreeHeight (rt)
binaryTreeHeight (BinaryTree { node=_, leftTree = Just lt, rightTree = Nothing } ) = 1 + binaryTreeHeight (lt)
binaryTreeHeight (BinaryTree { node=_, leftTree = Just lt, rightTree = Just rt } ) = 1 + max (binaryTreeHeight lt) (binaryTreeHeight rt)
---}}}


---{{{ An example tree
level8_1 = binaryLeaf "8_1"
level8_2 = binaryLeaf "8_2"
level8_3 = binaryLeaf "8_3"
level8_4 = binaryLeaf "8_4"
level8_5 = binaryLeaf "8_5"

level7_1 = binaryTree "7_1"  Nothing        (Just level8_1)
level7_2 = binaryTree "7_2" (Just level8_2)  Nothing
level7_3 = binaryTree "7_3"  Nothing        (Just level8_3)
level7_4 = binaryTree "7_4" (Just level8_4)  Nothing
level7_5 = binaryTree "7_5" (Just level8_5)  Nothing

level6_1 = binaryTree "6_1" (Just level7_1) (Just level7_2)
level6_2 = binaryTree "6_2" (Just level7_3) (Just level7_4)
level6_3 = binaryTree "6_3" (Just level7_5) Nothing

level5_1 = binaryTree "5_1" (Just level6_1) (Just level6_2)
level5_2 = binaryTree "5_2" Nothing  (Just level6_3)

level4_1 = binaryLeaf "4_1"
level4_2 = binaryTree "4_2" (Just level5_1) Nothing
level4_3 = binaryTree "4_3" Nothing  (Just level5_2)

level3_1 = binaryTree "3_1" Nothing (Just level4_1)
level3_2 = binaryTree "3_2" (Just level4_2) (Just level4_3)

level2_1 = binaryTree "2_1" (Just level3_1) (Just level3_2)
level2_2 = binaryLeaf "2_2"

root = binaryTree "root" (Just level2_1) (Just level2_2)

---}}}

--- }}}

---{{{ Point and Convex hull problems : #2.9-2.12

---{{{ #2.9
data Pt a = Pt (a, a) deriving (Show)

data Direction = Left|Right|Straight deriving (Show)

pt (x,y) = Pt (x,y)

px (Pt (x,y)) = x
py (Pt (x,y)) = y

---}}}

---{{{ Some additional functions (norm/dot/angleBetween/crossprod/cosa)
dot (Pt (x1,y1)) (Pt (x2,y2)) = x1*x2+y1*y2

-- | Cross product where points are interpreted as vectors
cross :: (Num t) => Pt t -> Pt t -> t
cross (Pt (x1,y1)) (Pt (x2,y2)) =  x1*y2-x2*y1

norm :: (Floating t) => Pt t -> t
norm (Pt (x,y)) = sqrt (x*x+y*y)

-- | Create a vector from the first point argument to the second.
vec :: (Num t) => Pt t -> Pt t -> Pt t
vec (Pt (x1,y1)) (Pt (x2,y2)) = pt (x2-x1,y2-y1)

-- | Calculates the cosine of the acute angle between pc to p1 and pc to p2
cosa pc p1 p2 = let u = vec pc p1
                    v = vec pc p2
                in ( (dot u v) / (norm u) / (norm v) )

-- | Calculates the acute angle between pc to p1 and pc to p2
angleBetween pc p1 p2 = acos ( cosa pc p1 p2 )

---}}}

---{{{#2.10
ccw :: (Num t) => Pt t -> Pt t -> Pt t -> t
ccw p1 p2 p3 = ((px p2) - (px p1))*((py p3) - (py p1)) - ((py p2) - (py p1))*((px p3) - (px p1))

direction :: (Num t, Ord t) => Pt t -> Pt t -> Pt t -> Direction
direction p1 p2 p3 | c > 0     = Left
                   | c < 0     = Right
                   | otherwise = Straight
                   where c = ccw p1 p2 p3
---}}}

---{{{ #2.11
findDirectionForAll :: (Num t, Ord t) => [Pt t] -> [Direction] 
findDirectionForAll []          = []
findDirectionForAll [p1]        = []
findDirectionForAll [p1,p2]     = []
findDirectionForAll (p1:p2:p3:ps) = [(direction p1 p2 p3)] ++ findDirectionForAll (p2:p3:ps)

---}}}

---{{{ #2.12 Graham scan algorithm

-- order points with by lowest y-value.  In cases where y values are equal, sort by lowest x value

-- | Compares two points by their y-coordinate, then their x-coordinate.  Lowest values first.
comparePointsByY :: (Ord t) => Pt t -> Pt t -> Bool
comparePointsByY p1 p2 = compareByLowY p1 p2

-- | Lowest y-coordinate first.  If they are equal, then compare by lowest x-coordinate
compareByLowY :: (Ord t) => Pt t -> Pt t -> Bool
compareByLowY p1 p2 | (py p1) <  (py p2) = True
                    | (py p1) == (py p2) = compareByLowX p1 p2
                    | otherwise          = False

-- | Lowest x-coordinate first. If they are equal, tehn return true.
compareByLowX :: (Ord t) => Pt t -> Pt t -> Bool
compareByLowX p1 p2 | (px p1) <  (px p2)  = True
                    | (px p1) == (px p2)  = True
                    | otherwise           = False

-- | Removes the third of a tuple and switches the first two elements.
remove3of3andSwitch :: (t, t1, t2) -> (t1, t)
remove3of3andSwitch (x,y,_) = (y,x)

-- got this from http://www.haskell.org/pipermail/beginners/2010-May/004282.html
-- Modified it so it returns the largest element.  Note that the order of the
-- original list is not preserved, but this is OK since we still have to 
-- order the list anyway.
removeElementTest :: (t -> t -> Bool) -> [t] -> (t,[t])
removeElementTest test (p:ps) = (remove3of3andSwitch (go [] p ps))
  where
    go ps   p []                   =    (ps,      p,  [])
    go plow p (p2:ps) | (test p2 p)  = go (p:plow)  p2 (ps)
                        | otherwise    = go (p2:plow) p  (ps)
removeElementTest _ _ = error "There is no elements in the passed list."

-- | Find the element with the lowest y value.  In the event that there
-- | is more than one element with a low y value, find the element
-- | with the lowest x value.  This will become your pivot element.
-- | Returns a tuple of the pivot element and the remainder of the list 
-- | without the pivot element. 
cHullStep1 :: (Ord t) => [Pt t] -> (Pt t, [Pt t])
cHullStep1 ps = removeElementTest comparePointsByY ps

-- | Calculate the angle (or a quantity that has the same ordering as the angle
-- | in the domain of 0 to 180 degrees), for each point with respect to the pivot
-- | element and the x-axis.
cHullStep2 :: (RealFloat t) => Pt t -> [Pt t] -> [(t, Pt t)]
cHullStep2 pvt [] = []
cHullStep2 pvt (p1:ps) = let Pt (vx,vy) = vec pvt p1
                    in [(atan2 vy vx, p1)] ++ cHullStep2 pvt ps

-- | Calculates the angle (or a quantity that has the same ordering as the angle
-- | in the domain of 0 to 180 degrees) for each element in ps as a tuple.  Returns
-- | a tuple of the pivot point and a list of these (angle, point) tuples.
doCHullStep2 :: (RealFloat t) => (Pt t, [Pt t]) -> (Pt t, [(t, Pt t)])
doCHullStep2 (pvt, ps) =(pvt, cHullStep2 pvt ps)

-- | Compares by the angle of the vector with the x-axis.  If the angles are equal,
-- | order by distance with the pivot element.
compareByAngle (a1,p1) (a2, p2) = case compare a1 a2 of
                                       LT -> LT
                                       EQ -> if comparePointsByY p1 p2 then LT else GT-- The same as comparing by distance.
                                       GT -> GT

-- | Sort the list by its angle with the pivot element.  In the event that two elements
-- | have the same angle, sort by distance with the pivot element.
cHullStep3 :: (Ord t, Ord t1) => [(t, Pt t1)] -> [(t, Pt t1)]
cHullStep3 ps = sortBy compareByAngle ps
doCHullStep3 (pvt,ps) = (pvt, cHullStep3 ps)

-- | Strips away and returns the first element of a tuple.
get1st :: (t, t1) -> t
get1st (a, _) = a

-- | Strips away and returns the second element of a tuple.
get2nd :: (t, t1) -> t1
get2nd (_, b) = b


-- | Take the pivot and (angle, point) tuples and strip away the angle.
-- | Returns the pivot and list of remaining ordered points.
cHullStep4 = get2nd.unzip
doCHullStep4 (pvt,ps) = (pvt, cHullStep4 ps)

noRightTurns Right = False
noRightTurns _     = True


-- | From wikipedia: 'The algorithm proceeds by considering each of the points in the sorted array in sequence. 
-- | For each point, it is determined whether moving from the two previously considered points to this point is 
-- | a "left turn" or a "right turn". If it is a "right turn", this means that the second-to-last point is not 
-- | part of the convex hull and should be removed from consideration. This process is continued for as long as 
-- | the set of the last three points is a "right turn". As soon as a "left turn" is encountered, the algorithm
-- | moves on to the next point in the sorted array.'
cHullStep5 :: (Num t, Ord t) => Pt t -> Pt t -> [Pt t] -> [Pt t] 
cHullStep5 p1 p2 []    = [p2] -- the last element will always be in the set.
cHullStep5 p1 p2 (p3:ps) = if p2madeRight 
                   -- If the second point is a right turn 
                   -- then it is part of the convex hull
                      then p2:(cHullStep5 p2 p3 ps)
                   -- otherwise it is not and should be removed from further consideration.
                      else    (cHullStep5 p1 p3 ps)
                      where p2madeRight = noRightTurns (direction p1 p2 p3)

-- | From wikipedia: 'The algorithm proceeds by considering each of the points in the sorted array in sequence. 
-- | For each point, it is determined whether moving from the two previously considered points to this point is 
-- | a "left turn" or a "right turn". If it is a "right turn", this means that the second-to-last point is not 
-- | part of the convex hull and should be removed from consideration. This process is continued for as long as 
-- | the set of the last three points is a "right turn". As soon as a "left turn" is encountered, the algorithm
-- | moves on to the next point in the sorted array.'
doCHullStep5 :: (Num t, Ord t) => (Pt t, [Pt t]) -> [Pt t]
doCHullStep5 (pvt,(p2:ps)) = pvt:(cHullStep5 pvt p2 ps) 

getDegenerateCHullCase (pvt, remaining) = pvt:remaining

-- | handle some degenerate cases
-- | I would also try to use nub to remove repeated points (which can screw this up I think), but
-- | I'm not sure how to overload the (==) operator for points, which is a requirement of nub.
calcCHull :: (RealFloat t, Ord t) => [Pt t] -> [Pt t]
calcCHull []         = []
calcCHull [p1,p2]    = getDegenerateCHullCase $ cHullStep1 [p1,p2]
calcCHull [p1,p2,p3] = getDegenerateCHullCase $ (doCHullStep4.doCHullStep3.doCHullStep2.cHullStep1) [p1,p2,p3]
calcCHull ps         = (doCHullStep5.doCHullStep4.doCHullStep3.doCHullStep2.cHullStep1) ps


-- Generate some data for testing purposes.

simpleTestPoints = [Pt (0.0,0.0), Pt (1.0,1.0), Pt (2.0,0.0), Pt (0.0,2.0), Pt (2.0,2.0)]
cTPsV = [8.0,4.0,4.0,3.0,3.0,6.0,1.0,3.0,9.0,6.0,5.0,5.0,11.0,5.0,11.0,2.0,9.0,1.0,7.0,2.0,2.0,1.0,6.0,1.0]

mapComplexTestPoints []         = []
mapComplexTestPoints (v1:v2:[]) = [Pt (v1, v2)] 
mapComplexTestPoints (v1:v2:vs) =  Pt (v1, v2) : mapComplexTestPoints vs

complexTestPoints = mapComplexTestPoints cTPsV

---}}}

---}}}

---}}}
