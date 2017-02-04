import Data.List
import Debug.Trace
import Data.Ord
import Data.Maybe

-- Problem 1
-- Return pair containing roots of quadratic equation a*x**2 + b*x + c.
-- The first element in the returned pair should use the positive
-- square-root of the discriminant, the second element should use the
-- negative square-root of the discriminant.  Need not handle complex
-- roots.
--- -------------------------------------------------------------------------------------------------------
--		 /*Comments : quadraticRoots*/
-- 1. stadard formula used for cacluating roots of the qudratic equation
-- 2. First class function used : sqrt
-----------------------------------------------------------------------------------------------------------
quadraticRoots :: Floating t => t -> t -> t -> (t, t)
quadraticRoots a b c = ((-b+sqrt(b*b-4*a*c))/(2*a), (-b-sqrt(b*b-4*a*c))/(2*a))
-----------------------------------------------------------------------------------------------------------
--		/*Comments : iterateFunction*/
-- 1. first line defines prototype or std definition.
-- 2. second line call the function iteratively passing two arguments and recursive calls.
-- 3. generates infinite list in the expected pattern.
-----------------------------------------------------------------------------------------------------------
-- Problem 2:
-- Return infinite list containing [z, f(z), f(f(z)), f(f(f(z))), ...]
-- May use recursion.
iterateFunction :: (a -> a) -> a -> [a]
iterateFunction f z = z : iterateFunction f (f z)
-----------------------------------------------------------------------------------------------------------
-- Problem 3
-- Using iterateFunction return infinite list containing
-- multiples of n by all the non-negative integers.
-- May NOT use recursion.
-----------------------------------------------------------------------------------------------------------
--		/*Comments : multiples n*/
-- 1. it calls the iterateFunction and passes the single argument
-----------------------------------------------------------------------------------------------------------
multiples n = iterateFunction (\x->x+n) 0
-----------------------------------------------------------------------------------------------------------
-- Problem 4
-- Use iterateFunction to return an infinite list containing list
-- of hailstone numbers starting with n.
-- Specifically, if i is a hailstone number, and i is even, then
-- the next hailstone number is i divided by 2; if i is a hailstone
-- number and i is odd, then the next hailstone number is 3*i + 1.
-- May NOT use recursion.
----------------------------------------------------------------------------------------------------------- 
--		/*Comments : hailstones*/
-- 1. it checks if number is even. If yes, then divides it by 2 and prints it
-- 2. otherwise every odd number is performed operation n * 3 + 1 and printed
-----------------------------------------------------------------------------------------------------------
hailstones :: Integral a => a -> [a]
hailstones n
	| even n = n : hailstones (head (tail (iterateFunction (\n -> n `div` 2) n)))
	| otherwise = n : hailstones (head (tail (iterateFunction (\n -> n * 3 + 1) n)))
-----------------------------------------------------------------------------------------------------------
-- Problem 5
-- Return length of hailstone sequence starting with n terminating
-- at the first 1.
-- May NOT use recursion.  Can use elemIndex from Data.List
----------------------------------------------------------------------------------------------------------- 
--		/*Comments : hailstonesLen*/
-- 1. This function uses the first class function fromJust
-- 2. To use fromJust, we need to import the library Maybe
-- 3. elemIndex is used to takeout the index of the element of the list 
-----------------------------------------------------------------------------------------------------------
hailstonesLen :: Integral a => a -> Int
hailstonesLen n = fromJust (elemIndex 1 (hailstones n))+ 1
-----------------------------------------------------------------------------------------------------------
-- Problem 6
-- Given a list of numbers, return sum of the absolute difference
-- between consecutive elements of the list.
-- May NOT use recursion.
sumAbsDiffs :: Num a => [a] -> a							-- definition
sumAbsDiffs numberList = sum (map abs $ (zipWith (-) numberList (tail numberList)))	-- maps the abs
											-- value of list 
-----------------------------------------------------------------------------------------------------------
-- Problem 7
-- The x-y coordinate of a point is represented using the pair (x, y).
-- Return the list containing the distance of each point in list
-- points from point pt.
-- May NOT use recursion.
distances :: Floating b => (b, b) -> [(b, b)] -> [b]
distances pt points = map (\(x, y) -> sqrt ((x - fst pt) * (x - fst pt) + (y - snd pt) * (y - snd pt))) points
											-- uses map function
											-- uses fst and snd
-----------------------------------------------------------------------------------------------------------
-- Problem 8
-- Given a list of coordinate pairs representing points, return the
-- sum of the lengths of all line segments between successive
-- adjacent points.
-- May NOT use recursion.
sumLengths :: Floating a => [(a, a)] -> a
sumLengths pointsList = sum (zipWith (\(x, y) (a, b)-> sqrt((x-a)*(x-a)+(y-b)*(y-b))) pointsList (tail pointsList))
-------------------------------------------------------------------------------------------------------------
-- Problem 9
-- Given a string s and char c, return list of indexes in s where c
-- occurs
occurrences s c = elemIndices c s
-------------------------------------------------------------------------------------------------------------

-- A tree of some type t is either a Leaf containing a value of type t,
-- or it is an internal node (with constructor Tree) with some left
-- sub-tree, a value of type t and a right sub-tree.
data Tree t = Leaf t
            | Tree (Tree t) t (Tree t)

-- Problem 10
-- Fold tree to a single value. If tree is a Tree node, then it's
-- folded value is the result of applying ternary treeFn to the result
-- of folding the left sub-tree, the value stored in the Tree node and
-- the result of folding the right sub-tree; if it is a Leaf node,
-- then the result of the fold is the result of applying the unary
-- leafFn to the value stored within the Leaf node.
-- May use recursion.
foldTree :: (t1 -> t -> t1 -> t1) -> (t -> t1) -> Tree t -> t1
foldTree treeFn leafFn (Leaf tree) = leafFn tree
foldTree treeFn leafFn (Tree left v right) = treeFn (foldTree treeFn leafFn left) v (foldTree treeFn leafFn right)
--foldTree tree + t + foldTree tree
--------------------------------------------------------------------------------------------
-- Problem 11
-- Return list containing flattening of tree.  The elements of the
-- list correspond to the elements stored in the tree ordered as per
-- an in-order traversal of the tree. Must be implemented using foldTree.
-- May NOT use recursion.
flattenTree :: Tree a -> [a]
flattenTree tree = foldTree (\l x r -> l ++ [x] ++ r) (\x->[x]) tree
--------------------------------------------------------------------------------------------
-- Problem 12
-- Given tree of type (Tree [t]) return list which is concatenation
-- of all lists in tree.
-- Must be implemented using flattenTree.
-- May NOT use recursion.
catenateTreeLists :: Tree [a] -> [a]
catenateTreeLists tree = foldr (++) [] (flattenTree tree)
---------------------------------------------------------------------------------------------
