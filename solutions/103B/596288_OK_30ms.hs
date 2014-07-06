import Data.Graph
main = do
  [n, m]:es <- fmap (map (map read.words).lines) getContents
  let g = buildG (1,n) $ concat [[(x,y),(y,x)] | [x,y] <- es]
  let c = length (components g)
  putStrLn $ if n == m && c == 1 then "FHTAGN!" else "NO"
