import Data.List

-- Problem 1: Find the last element of a List

-- Solution #1
lastListElement1 :: [a] -> a
lastListElement1 [] = error "Empty list"
lastListElement1 xs = xs !! (length xs - 1)

-- Solution #2
lastListElement2 :: [a] -> a
lastListElement2 [] = error "Empty list"
lastListElement2 xs = head (reverse xs)

-- Problem 2: Find the last but one element of a list
previousOfLastListElement1 :: [a] -> a
previousOfLastListElement1 xs = reverse xs !! 1

previousOfLastListElement2 :: [a] -> a
previousOfLastListElement2 xs = last (init xs)

-- User composition of functions
previousOfLastListElement3 :: [a] -> a
previousOfLastListElement3 = last . init

-- Problem 3: Find the K'th element of a list. The first element in the list is number 1
findElementInList :: Int -> [a] -> a
findElementInList 0 _ = error "Index out of bounds"
findElementInList _ [] = error "Empty list"
findElementInList k xs = xs !! (k-1)

-- Problem 4: Find the number of elements of a list
listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

listLength2 :: [a] -> Int
listLength2 xs = length xs

-- Problem 5: Reverse a list
-- Use Haskell library
reverseList :: [a] -> [a]
reverseList = reverse

reverseList2 :: [a] -> [a]
reverseList2 [] = []
reverseList2 (x:xs) = reverseList2 xs ++ [x]

-- Problem 6: Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome (x:[]) = True
palindrome xs = (head xs == last xs) && (palindrome . init . tail) xs

-- Problem 7: Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- With concatMap: function which apply a function on all elements of a list and then merge the results
flatten2 :: NestedList a -> [a]
flatten2 (Elem x) = [x]
flatten2 (List x) = concatMap flatten2 x

-- Problem 8: Eliminate consecutive duplicates of list elements.
eliminateConsecutive :: (Eq a) => [a] -> [a]
eliminateConsecutive (x:[]) = [x]
eliminateConsecutive (x:xs) = if (x == head xs)
  then eliminateConsecutive xs
  else [x] ++ eliminateConsecutive xs

-- Problem 9: Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.

-- use Haskell Data.List group function
packDuplicates :: (Eq a) => [a] -> [[a]]
packDuplicates = group

duplicateHelper :: (Eq a) => Int -> [a] -> [[a]]
duplicateHelper y (x:[]) = [replicate y x]
duplicateHelper y (x:xs) = if (x == head xs)
  then duplicateHelper (y+1) xs
  else [replicate y x] ++ duplicateHelper 1 xs

packDuplicates2 :: (Eq a) => [a] -> [[a]]
packDuplicates2 xs = duplicateHelper 1 xs

-- Soution using span function
packDuplicates3 :: (Eq a) => [a] -> [[a]]
packDuplicates3 [] = []
packDuplicates3 (x:xs) = let
  (first, rest) = span (==x) xs
  in (x:first) : packDuplicates3 rest

-- Problem 10: Run-length encoding of a list. Use the result of problem P09 to implement the so-called
-- run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E)
-- where N is the number of duplicates of the element E.

-- use previous function packDuplicates
countElements :: [a] -> (Int, a)
countElements xs = (length xs, head xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map countElements (packDuplicates xs)

-- inline lambda and compose with group
encode2 :: (Eq a) => [a] -> [(Int, a)]
encode2 = map (\x -> (length x, head x)) . group

-- Problem 11: Modified run-length encoding. Modify The result of problem 10 in such a way that
--  if an element has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.
data GeneralTuple a = Single a | Multiple Int a

instance Show a => Show (GeneralTuple a) where
  show (Single a) = show ("Single " ++ show a)
  show (Multiple k a) = show ("Multiple " ++ show k ++ " " ++ show a)

encodeTuple :: [a] -> GeneralTuple a
encodeTuple xs = if length xs == 1
  then Single (head xs)
  else Multiple (length xs) (head xs)

encodeModified :: (Eq a) => [a] -> [GeneralTuple a]
encodeModified = map encodeTuple . group

-- Problem 12: Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

decodeGeneralTuple :: GeneralTuple a -> [a]
decodeGeneralTuple (Single x) = [x]
decodeGeneralTuple (Multiple k x) = replicate k x

decodeModified :: [GeneralTuple a] -> [a]
decodeModified xs = concatMap decodeGeneralTuple xs

-- Problem 13: (**) Run-length encoding of a list (direct solution).
-- The so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.


-- Problem 14: (*) Duplicate the elements of a list.
duplication :: [a] -> [a]
duplication [] = []
duplication (x:xs) = [x, x] ++ duplication xs

-- Problem 15: (**) Replicate the elements of a list a given number of times.
-- Using lib function
repli :: Int -> [a] -> [a]
repli _ [] = []
repli k xs = concat (map (replicate k) xs)

-- From scratch using list comprehension
repli2 :: Int -> [a] -> [a]
repli2 _ [] = []
repli2 k (x:xs) = [x | y<-[1,k]] ++ repli2 k xs

-- Problem 16: (**) Drop every N'th element from a list.
-- dropEveryN :: Int -> [a] -> [a]
-- dropEveryN k xs = take (k-1) xs ++ dropEveryN k (drop k xs)

-- Problem 17: (*) Split a list into two parts; the length of the first part is given.

-- Problem 18: (**) Extract a slice from a list.

-- Problem 19: (**) Rotate a list N places to the left.

-- Problem 20: (*) Remove the K'th element from a list.
