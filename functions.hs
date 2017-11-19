import Data.List

-- Split an even-sized list in 2 parts
halve :: [a] -> ([a], [a])
halve xs
  | (length xs `mod` 2 == 0) = (ys, zs)
  | (length xs `mod` 2 == 1) = ([], [])
  where
    ys = take ((length xs) `div` 2) xs
    zs = drop ((length xs) `div` 2) xs


-- Return the third element of a list
third :: [a] -> Maybe a
third xs
  | length xs < 3 = Nothing
  | length xs >= 3 = Just(xs !! 2)

thirdTwo :: [a] -> Maybe a
thirdTwo xs
  | length xs < 3 = Nothing
  | length xs >= 3 = Just(head (tail (tail xs)))

thirdThree :: [a] -> Maybe a
thirdThree (_:_:x:xs) = Just(x)
thirdThree (_) = Nothing

-- Coding a safe tail function
safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

safeTailTwo :: [a] -> [a]
safeTailTwo xs
  | length xs == 0 = []
  | otherwise = tail xs

safeTailThree :: [a] -> [a]
safeTailThree xs = if length xs == 0
  then []
  else tail xs
