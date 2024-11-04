sort [] = []
sort (x : xs) = sort lo ++ [x] ++ sort hi
  where
    lo = [y | y <- xs, y < x]
    hi = [y | y <- xs, y >= x]

main = do
  print $ sort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
