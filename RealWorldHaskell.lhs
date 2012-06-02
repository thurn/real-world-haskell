\documentclass{article}
%include polycode.fmt
\begin{document}

% Insert a blank line between paragraphs:
\addtolength{\parskip}{\baselineskip}

\title{\underline{Real World Haskell} Solutions}
\author{Derek Thurn}

\maketitle

This file contains solutions to the Exercises found in the book \underline{Real
World Haskell}, presented in the form of a Literate Haskell file.

\section*{Preface}

We start with a Haskell module declaration and the imports we'll be using
throughout this solution set.

\begin{code}
module RealWorldHaskell where

import Data.List (genericLength, sortBy, delete)
import Data.Ord (comparing)
import Data.Char (digitToInt, isDigit, isSpace)
\end{code}

\section*{Chapter 1: Getting Started}

\subsection*{1)}
Enter the following expressions into \textbf{ghci}. What are their types?
\begin{itemize}
\item \texttt{5 + 8 :: Integer}
\item \texttt{3 * 5 + 8 :: Integer}
\item \texttt{2 + 4 :: Integer}
\item \texttt{(+) 2 4 :: Integer}
\item \texttt{sqrt 16 :: Double}
\item \texttt{succ 6 :: Integer}
\item \texttt{succ 7 :: Integer}
\item \texttt{pred 9 :: Integer}
\item \texttt{pred 8 :: Integer}
\item \texttt{sin (pi / 2) :: Double}
\item \texttt{truncate pi :: Integer}
\item \texttt{round 3.5 :: Integer}
\item \texttt{round 3.4 :: Integer}
\item \texttt{floor 3.7 :: Integer}
\item \texttt{ceiling 3.3 :: Integer}
\end{itemize}

\subsection*{2)}
From \textbf{ghci}, type \textbf{:?} to print some help. Define a variable,
such as \texttt{let x = 1}, then type \texttt{:show bindings}. What do you see?

\begin{verbatim}
Prelude> let x = 1
Prelude> :show bindings
x :: Integer = 1
\end{verbatim}

\subsection*{3)}
The \texttt{words} function counts the number of words in a string. Modify the
\texttt{WC.hs} example to count the number of words in a file.

\begin{code}
countWordsInInput :: IO ()
countWordsInInput = interact (show . countWordsInString)

countWordsInString :: String -> Int
countWordsInString = length . words
\end{code} 

\subsection*{4)}
Modify the \texttt{WC.hs} example again, to print the number of characters in a
file.

\begin{code}
countCharactersInInput :: IO ()
countCharactersInInput = interact (show . countCharactersInString)

countCharactersInString :: String -> Int
countCharactersInString = length
\end{code}

\section*{Chapter 2: Types and Functions -- First Set}

\subsection*{1)}

What are the types of the following expressions?
\begin{itemize}
\item \texttt{False :: Bool}
\item \texttt{(["foo", "bar"], 'a') :: ([String], Char)}
\item \texttt{[(True, []), (False, [['a']])] :: [(Bool, [[Char]])]}
\end{itemize}

\section*{Chapter 2: Types and Functions -- Second Set}

\subsection*{1)}
Haskell provides a standard function, \texttt{last :: [a] -> a}, that returns
the last element of a list. From reading the type alone, what are the possible
valid behaviours (omitting crashes and infinite loops) that this function could
have? What are a few things that this function clearly cannot do?

\begin{itemize}
\item \texttt{last} must return an element from the list it gets as input.
\item It cannot, e.g., perform IO without resorting to something like
\texttt{unsafePerformIO}
\end{itemize}

\subsection*{2)}
Write a function \texttt{lastButOne}, that returns the element \emph{before}
the last.

\begin{code}
lastButOne :: [a] -> Maybe a
lastButOne xs  | length xs > 1  = Just $ (last .init) xs
               | otherwise      = Nothing
\end{code}

\subsection*{3)}
Load your \texttt{lastButOne} function into \textbf{ghci}, and try it out on
lists of different
lengths. What happens when you pass it a list that's too short?
\begin{verbatim}
Prelude> :l RealWorldHaskell
[1 of 1] Compiling RealWorldHaskell ( RealWorldHaskell.lhs, interpreted )
Ok, modules loaded: RealWorldHaskell.
*RealWorldHaskell> lastButOne [1, 2]
Just 1
*RealWorldHaskell> lastButOne [1]
Nothing
\end{verbatim}

\section*{Chapter 3: Defining Types, Streamlining Functions -- First Set}

\subsection*{1)}
Write the converse of \texttt{fromList} for the \texttt{List} type: a function
that takes a
\texttt{List a} and generates a \texttt{[a]}.

\begin{code}
data List a  =  Cons a (List a)
             |  Nil
                deriving (Show)
              
toList :: List a -> [a]
toList Nil          = []
toList (Cons x xs)  = x : toList xs
\end{code}

\subsection*{2)}
Define a tree type that has only one constructor, like our Java example.
Instead of the \texttt{Empty} constructor, use the \texttt{Maybe} type to refer
to a node's children.

\begin{code}
data MaybeTree a = MaybeTree (Maybe a) (Maybe a)
\end{code}

\section*{Chapter 3: Defining Types, Streamlining Functions -- Second Set}

\subsection*{1)}
Write a function that computes the number of elements in a list. To test it,
ensure that it gives the same answers as the standard \texttt{length} function.

\begin{code}
numElements []      = 0
numElements (_:xs)  = 1 + numElements xs
\end{code}

\subsection*{2)}
Add a type signature for your function to your source file. To test it, load
the source file into \textbf{ghci} again.

\begin{code}
numElements :: [a] -> Int
\end{code}

\subsection*{3)}
Write a function that computes the mean of a list, i.e. the sum of all elements
in the list divided by its length. (You may need to use the
\texttt{fromIntegral} function to convert the length of the list from an 
integer into a floating point number.)

\begin{code}
listMean ::  (Real a, Fractional b) => [a] -> b
listMean []  = 0.0
listMean xs  = realToFrac (sum xs) / genericLength xs
\end{code}

\subsection*{4)}
Turn a list into a palindrome, i.e. it should read the same both backwards and
forwards. For example, given the list \texttt{[1,2,3]}, your function should
return \texttt{[1,2,3,3,2,1]}.

\begin{code}
toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ reverse xs
\end{code}

\subsection*{5)}
Write a function that determines whether its input list is a palindrome.

\begin{code}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = reverse xs == xs
\end{code}

\subsection*{6)}
Create a function that sorts a list of lists based on the length of each
sublist. (You may want to look at the \texttt{sortBy} function from the
\texttt{Data.List} module.)

\begin{code}
sortListsByLength :: [[a]] -> [[a]]
sortListsByLength = sortBy (comparing length)
\end{code}

\subsection*{7)}
Define a function that joins a list of lists together using a separator value.
The separator should appear between elements of the list, but should not follow
the last element. Your function should behave as follows:

\begin{verbatim}
ghci> :load Intersperse
[1 of 1] Compiling Main             ( Intersperse.hs, interpreted )
Ok, modules loaded: Main.
ghci> intersperse ',' []
""
ghci> intersperse ',' ["foo"]
"foo"
ghci> intersperse ',' ["foo","bar","baz","quux"]
"foo,bar,baz,quux"
\end{verbatim}

\begin{code}
joinWithSeparator :: a -> [[a]] -> [a]
joinWithSeparator _ []          = []
joinWithSeparator _ [x]         = x
joinWithSeparator sep (xs:xss)  = xs ++ sep : joinWithSeparator sep xss
\end{code}

\subsection*{8)}
Using the binary tree type that we defined earlier in this chapter, write a
function that will determine the height of the tree. The height is the largest
number of hops from the root to an \texttt{Empty}. For example, the tree
\texttt{Empty} has height zero; \texttt{Node "x" Empty Empty} has height one;
\texttt{Node "x" Empty (Node "y" Empty Empty)} has height two; and so on.

\begin{code}
data Tree a  =  Node a (Tree a) (Tree a)
             |  Empty
                deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty           = 0
treeHeight (Node _ t1 t2)  = 1 + max (treeHeight t1) (treeHeight t2)
\end{code}

\subsection*{9)}
Consider three two-dimensional points $a$, $b$, and $c$. If we look at the
angle formed by the line segment from $a$ to $b$ and the line segment from
$b$ to $c$, it either turns left, turns right, or forms a straight line.
Define a \texttt{Direction} data type that lets you represent these
possibilities.

\begin{code}
data Direction  =  TurnsLeft
                |  TurnsRight
                |  Straight
                   deriving (Show, Eq)
\end{code}

\subsection*{10)}
Write a function that calculates the turn made by three 2D points and returns a
\texttt{Direction}.

\begin{code}
type Point = (Double, Double)

calculateTurn :: Point -> Point -> Point -> Direction
calculateTurn (x1, y1) (x2, y2) (x3, y3)  | crossProduct < 0    = TurnsRight
                                          | crossProduct > 0    = TurnsLeft
                                          | otherwise  = Straight
  where crossProduct = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
\end{code}

\subsection*{11)}
Define a function that takes a list of 2D points and computes the direction of
each successive triple. Given a list of points \texttt{[a,b,c,d,e]}, it should
begin by computing the turn made by \texttt{[a,b,c]}, then the turn made by
\texttt{[b,c,d]}, then \texttt{[c,d,e]}. Your function should return a list of
\texttt{Direction}.

\begin{code}
turnList :: [Point] -> [Direction]
turnList (p1:p2:p3:ps)  = calculateTurn p1 p2 p3 : turnList (p2 : p3 : ps)
turnList _              = []
\end{code}

\subsection*{12)}

Using the code from the preceding three exercises, implement Graham's scan
algorithm for the convex hull of a set of 2D points. You can find a good
description of what a convex hull is, and how the Graham scan algorithm should
work, on Wikipedia.

The first step in this algorithm is to find the point with the lowest
y-coordinate. If the lowest y-coordinate exists in more than one point in the
set, the point with the lowest x-coordinate out of the candidates should be
chosen. Call this point $P$. The function $findP$ locates point P.

\begin{code}
findP :: [Point] -> Point
findP = head . (sortBy yThenX)
  where yThenX (x1, y1) (x2, y2) = compare (y1, x1) (y2, x2)
\end{code}

\pagebreak

Next, the set of points must be sorted in increasing order of the angle they
and the point $P$ make with the x-axis. The function $sortByAngle$
accomplishes this by comparing the cotangent of the angle, which is
proportional to the angle itself.

\begin{code}
sortByAngle :: [Point] -> [Point]
sortByAngle ps = pointP : sortBy (comparing cotan) (delete pointP ps)
  where  pointP@(px, py)  = findP ps
         cotan (x, y)     = (px - x) / (y - py)
\end{code}
 
Next, we need a new datatype. A $PointMap$ is a mapping from a point to its
$Direction$.

\begin{code}
type PointMap = [(Point, Direction)]
\end{code}

To make a $PointMap$, the $makePointMap$ function maps each point to the
$Direction$ it makes with the point before it and at the point after it in the
input point list. In order to get the result to ``wrap around'' we stick the
last point at the start of the input to $turnList$ and the first point at the
end.

\begin{code}
makePointMap :: [Point] -> PointMap
makePointMap ps = zip ps $ turnList $ [last ps] ++ ps ++ [head ps]
\end{code}

To get the points out of a $PointMap$, we use $unzip$ to get a
$([Point], [Direction])$ tuple and then call $fst$ on the result.

\begin{code}
getPoints :: PointMap -> [Point]
getPoints = fst . unzip
\end{code}

Finally, we need a quick helper predicate. $rightTurn$ checks whether or not
a given point makes a right turn with respect to the point before it and the
point after it.

\begin{code}
rightTurn :: (Point, Direction) -> Bool
rightTurn (_, TurnsRight)  = True
rightTurn _                = False 
\end{code}

Now we are ready for the core Graham Scan algorithm. The algorithm proceeds by
considering each of the points in the sorted list in sequence. For each point,
it is determined whether the point makes a ``left turn'' or a ``right turn''
with respect to the point before it and the point after it. If the point makes
a right turn, it is not part of the convex hull, and should be removed.

The recursive $gscan$ function has two cases. In the base case, none of the
input points are mapped to a right turn, meaning the input point map is the
convex hull, so we return. Otherwise, the function discards the right turns
from the input list and re-computes the directions for the new point list,
recursing on the result so all new right turns can again be eliminated. 

\begin{code}
gscan :: PointMap -> PointMap
gscan pointMap  | all (not . rightTurn) pointMap  = pointMap
                | otherwise = gscan $ makePointMap points
  where points  = getPoints $ filter (not . rightTurn) pointMap
\end{code}

Finally, we need a top-level $convexHull$ function to string it all together.
We sort the input points by angle, convert them into a point map with
the correct $Direction$s, recursively eliminate right turns from the point
map until there are no more right turns, and then extract the points from the
result. 

\begin{code}
convexHull :: [Point] -> [Point]
convexHull ps = getPoints $ gscan $ makePointMap $ sortByAngle ps
\end{code}

\section*{Chapter 4: Functional programming -- First Set}

\subsection*{1)}
Write your own ``safe'' definitions of the standard partial list functions, but
make sure that yours never fail. As a hint, you might want to consider using
the following types:

\begin{verbatim}
-- file: ch04/ch04.exercises.hs
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]
\end{verbatim}

\begin{code}
makeSafe :: ([a] -> b) -> [a] -> Maybe b
makeSafe _ []   = Nothing
makeSafe fn xs  = Just $ fn xs

safeHead :: [a] -> Maybe a
safeHead = makeSafe head

safeTail :: [a] -> Maybe [a]
safeTail = makeSafe tail

safeLast :: [a] -> Maybe a
safeLast = makeSafe last

safeInit :: [a] -> Maybe [a]
safeInit = makeSafe init
\end{code}

\subsection*{2)}

Write a function \texttt{splitWith} that acts similarly to \texttt{words}, but
takes a predicate and a list of any type, and splits its input list on every
element for which the predicate returns \texttt{False}.

\begin{verbatim}
-- file: ch04/ch04.exercises.hs
splitWith :: (a -> Bool) -> [a] -> [[a]]
\end{verbatim}

\begin{code}
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith fn xs | null start = []
                | otherwise  = start : splitWith fn rest
  where (start, rest) = break fn (dropWhile fn xs)
\end{code}

\subsection*{3)}
Using the command framework from the section called ``A simple command line
framework'', write a program that prints the first word of each line of its
input.

\begin{code}
firstWords :: String -> String
firstWords = unlines . map (unwords . (take 1) . words) . lines
\end{code}
\pagebreak
\subsection*{4)}
Write a program that transposes the text in a file.

\begin{code}
transposeText :: String -> String
transposeText st = transp (lines st)
  where  transp xs  | all null xs  = ""
                    | otherwise    = heads xs ++ "\n" ++ transp (tails xs)
         heads = concat . map (take 1)
         tails = map (drop 1)
\end{code}

\section*{Chapter 4: Functional programming -- Second Set}

\subsection*{1)}

Use a fold (choosing the appropriate fold will make your code much simpler) to
rewrite and improve upon the \texttt{asInt} function from the section called
``Explicit recursion''.

\begin{verbatim}
-- file: ch04/ch04.exercises.hs
asInt_fold :: String -> Int
\end{verbatim}

Your function should behave as follows.

\begin{verbatim}
ghci> asInt_fold "101"
101
ghci> asInt_fold "-31337"
-31337
ghci> asInt_fold "1798"
1798
\end{verbatim}

Extend your function to handle the following kinds of exceptional conditions by
calling \texttt{error}.

\begin{verbatim}
ghci> asInt_fold ""
0
ghci> asInt_fold "-"
0
ghci> asInt_fold "-3"
-3
ghci> asInt_fold "2.7"
*** Exception: Char.digitToInt: not a digit '.'
ghci> asInt_fold "314159265358979323846"
564616105916946374
\end{verbatim}

\begin{code}
asInt_fold :: String -> Int
asInt_fold ('-':xs)  = negate $ asInt_fold xs
asInt_fold s         = foldl toInt 0 s
  where toInt accum val = accum * 10 + digitToInt val
\end{code}

\subsection*{2)}

The \texttt{asInt\_fold} function uses error, so its callers cannot handle
errors. Rewrite it to fix this problem.

\begin{verbatim}
-- file: ch04/ch04.exercises.hs
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int

ghci> asInt_either "33"
Right 33
ghci> asInt_either "foo"
Left "non-digit 'o'"
\end{verbatim}

\begin{code}
type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either ('-':xs)  = case asInt_either xs of
  (Right v)  -> Right $ negate v
  err        -> err
asInt_either s              = foldl toInt (Right 0) s
  where toInt (Left err) _  = Left err
        toInt (Right accum) val
          | isDigit val  = Right $ accum * 10 + digitToInt val
          | otherwise    = Left $ "non-digit '" ++ [val] ++ "'"
\end{code}

\subsection*{3)}
The Prelude function \texttt{concat} concatenates a list of lists into a single
list, and has the following type.

\begin{verbatim}
-- file: ch04/ch04.exercises.hs
concat :: [[a]] -> [a]
\end{verbatim}

Write your own definition of \texttt{concat} using \texttt{foldr}.

\begin{code}
foldConcat :: [[a]] -> [a]
foldConcat = foldr (++) []
\end{code}

\subsection*{4)}

Write your own definition of the standard \texttt{takeWhile} function, first
using explicit recursion, then \texttt{foldr}

\begin{code}
takeWhileRec :: (a -> Bool) -> [a] -> [a]
takeWhileRec _ []                   = []
takeWhileRec p (x:xs)  | p x        = x : takeWhileRec p xs
                       | otherwise  = []

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold p = foldr step []
  where step x accum  | p x        = x : accum
                      | otherwise  = []
\end{code}

\subsection*{5)}

The \texttt{Data.List} module defines a function, \texttt{groupBy}, which has
the following type.


\begin{verbatim}
-- file: ch04/ch04.exercises.hs
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
\end{verbatim}

Use \textbf{ghci} to load the \texttt{Data.List} module and figure out what
\texttt{groupBy} does, then write your own implementation using a fold.

\begin{code}
-- (ignoring cases where eq is not an equivalence relation)

groupByFold :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFold eq  = foldr step []
  where step x (ys:yss)  | x `eq` head ys  = (x : ys) : yss
                         | otherwise       = [x] : ys : yss
        step x []  = [[x]]
\end{code}

\subsection*{6)}

How many of the following Prelude functions can you rewrite using list folds?

\begin{itemize}
\item \texttt{any}
\begin{code}
anyFold :: (a -> Bool) -> [a] -> Bool
anyFold p = foldr ((||) . p) False
\end{code}

\item \texttt{cycle}
\begin{code}
cycleFold :: [a] -> [a]
cycleFold = foldr (++) [] . repeat
\end{code}

\item \texttt{words}
\begin{code}
wordsFold :: String -> [String]
wordsFold = dropWhile null . foldr step [""]
  where step x acc@([]:yss)  | isSpace x  = acc
                             | otherwise  = [x] : yss
        step x acc@(ys:yss)  | isSpace x  = [] : acc
                             | otherwise  = (x : ys) : yss
        step _ _ = error "invalid arguments to step"
\end{code}

\item \texttt{unlines}
\begin{code}
unlinesFold :: [String] -> String
unlinesFold = undefined
\end{code}
\end{itemize}











\end{document}