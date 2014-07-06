
mersenne = [2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 607, 1279, 2203, 2281, 3217, 4253, 4423, 9689, 9941, 11213, 19937, 21701, 23209, 44497, 86243, 110503, 132049, 216091, 756839, 859433, 1257787, 1398269, 2976221, 3021377, 6972593, 13466917, 20996011, 24036583]

modulus = 10^9 + 7 :: Integer

pow2 :: Integer -> Integer
pow2 0 = 1
pow2 n | even n = let a = pow2 (n `div` 2) in a * a `mod` modulus
pow2 n = pow2 (n - 1) * 2 `mod` modulus

solve :: Int -> Integer
solve n = (pow2 (mersenne !! (n - 1) - 1) - 1) `mod` modulus

main = interact $ show . solve . read
