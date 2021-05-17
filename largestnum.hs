delete_best :: [Int] -> [Int]
delete_best [n] = []
delete_best (n1:n2:ns) = if (n1 < n2) then [n2]++ns
                         else [n1]++delete_best (n2:ns)

largestnum :: [Int] -> Int -> [Int]
largestnum ns 0 = ns
largestnum ns r = largestnum (delete_best ns) (r-1)
