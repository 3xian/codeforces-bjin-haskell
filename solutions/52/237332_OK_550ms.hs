
import Data.List
import Data.Int

solve arr = sum $ zipWith (*) row $ map (sum . zipWith (\x y -> if y=='*' then x else 0) col) arr
  where
    mapping :: [[Char]] -> [Int64]
    mapping = map ((\x -> fromIntegral x - 1) . length . filter (=='*'))
    row = mapping arr
    col = mapping $ transpose arr

main = do
    arr <- fmap (tail.lines) getContents
    print $ solve arr
