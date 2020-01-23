import Data.List

-- | Some implementations of sorting algorithms

-- | Checks if a list is sorted in ascending order
sortedA :: (Ord a) => [a] -> Bool
sortedA [] = True
sortedA (x:[]) = True
sortedA (x:y:xs) =  (not (x>y)) && (sortedA (y:xs))

-- | My own implementation of minimimum
myMin :: (Ord a) => [a] -> a
myMin [] = error "empty list has no minimum"
myMin (x:[]) = x
myMin (x:y:[])
	| y<x = y
	| otherwise = x
myMin (x:xs) = myMin ([x] ++ [myMin xs])

-- | Merge sorted lists into a sorted list
merge ::  (Ord a) => [a] -> [a] -> [a]
merge ary [] = ary
merge [] ary = ary
merge (x:xs) (y:ys)
	| y<x = y:(merge (x:xs) ys)
	| otherwise = x:(merge xs (y:ys)) 

-- | Quicksort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:[]) = (x:[])
quickSort (x:y:[])
	| y<x = (y:x:[])
	| otherwise = (x:y:[])
quickSort (x:xs) = (quickSort (filter (<x) xs)) ++ (filter (==x) (x:xs)) ++ (quickSort (filter (>x) xs))

-- | Selection sort
selectSort :: (Ord a) => [a] -> [a]
selectSort [] = []
selectSort (x:[]) = (x:[])
selectSort ary = m:(selectSort (delete m ary)) 
	where m = minimum ary

-- | Auxilliary function to bubble sort
preBubbleSort :: (Ord a) => [a] -> [a]
preBubbleSort [] = []
preBubbleSort (x:[]) = (x:[])
preBubbleSort (x:y:xs)
	| y<x = y:(preBubbleSort (x:xs))
	| otherwise = x:(preBubbleSort (y:xs))

-- | Bubble sort
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort ary
	| sortedA ary = ary
	| otherwise = bubbleSort (preBubbleSort ary)

-- | Merge sort
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort (x:[]) = (x:[])
mergeSort (x:y:[])
	| y<x = (y:x:[])
	| otherwise = (x:y:[])
mergeSort ary = merge aryFront aryBack
	where n = quot (length ary) 2
	      aryFront = mergeSort (take n ary)
	      aryBack = mergeSort (drop n ary) 
