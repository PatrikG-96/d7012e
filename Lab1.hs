-- Patrik Guthenberg Lab 1 D7012E

data Sublist = Sublist {
    list_sum :: Int,
    start :: Int,
    end :: Int,
    list :: [Int]
} deriving (Show)

main = do
    let xs = [x*(-1)^x | x <- [1..100]]
    --let xs = [24,-11,-34,42,-24,7,-19,21]
    --let xs = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3]
    let k = 15
    smallestKsets k xs

smallestKsets :: Int -> [Int] -> IO()
smallestKsets k xs = putStr("Entire list: " ++ show xs ++ "\n\nsize\ti\tj\tsublist\n" ++ format (k_smallest k xs))

format :: [Sublist] -> String
format s = unlines (map sublist_str s)

sublist_str :: Sublist -> String
sublist_str s = show (list_sum s) ++ "\t" ++ show (start s) ++ "\t" ++ show(end s) ++ "\t" ++ show (list s)

k_smallest :: Int -> [Int]  -> [Sublist]
k_smallest k n = take k (quicksort (all_sublists n 0 (length n)))

all_sublists :: [Int] -> Int -> Int -> [Sublist]
all_sublists [] _ _ = []
all_sublists n i j = sublist2 n i j ++ all_sublists (tail n) (i+1) j

sublist2 :: [Int] -> Int -> Int -> [Sublist]
sublist2 [] _ _ = []
sublist2 n i j = [Sublist {list_sum = (foldr (+) 0 n), start = (i+1), end = j, list = n}] ++ sublist2 (init n) i (j-1)

quicksort :: [Sublist] -> [Sublist]
quicksort [] = []
quicksort (x:xs) = quicksort [s  | s <- xs, list_sum s <= list_sum x] ++ [x] ++ quicksort[s  | s <- xs, list_sum s > list_sum x]
  


