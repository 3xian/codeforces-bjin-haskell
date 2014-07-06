import List
main = interact $ show.s.sort.map read.tail.words
s xs = let n = length xs in n - (length $ group $ filter (<=n) xs)
