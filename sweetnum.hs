sweet :: [Int] -> Bool
sweet [n] = True
sweet (n1:n2:ns) | ((mod n1 2 == 0) && (mod n2 2 == 0)) = False
                 | ((mod n1 2 == 1) && (mod n2 2 == 1)) = False
                 | otherwise = sweet(n2:ns)

change_up :: Int -> Int
change_up 9 = 8
change_up n = n+1

change_down :: Int -> Int
change_down 0 = 1
change_down n = n-1

sweetify :: [Int] -> Int -> [Int]
sweetify (n1:n2:ns) flag
                 | sweet ([n1]++[n2]) = [n1]++sweetify(n2:ns) flag
                 | otherwise = if (flag == 1) then [change_up n1]++[n2]++ns
                                              else [change_down n1]++[n2]++ns

find_sweet :: [Int] -> [Int] -> ([Int], [Int])
find_sweet a b | (sweet a && sweet b) = (a, b)
               | sweet a = (a, a)
               | sweet b = (b, b)
               | otherwise = find_sweet (sweetify a 0) (sweetify b 1)

list_to_int :: [Int] -> Int -> Int
list_to_int [] _ = 0
list_to_int (d:ds) b = b*d + (list_to_int ds (b*10))

diff :: Int -> Int -> Int
diff a b = a - b

sweetnum :: [Int] -> ([Int], [Int])
sweetnum ns  
             | (d1 == d2) = (rev_a, rev_b)
             | (d1 < d2) = (rev_a, rev_a)
             | otherwise = (rev_b, rev_b)
             where rev_ns = reverse ns
                   (a, b) = find_sweet rev_ns rev_ns
                   int_a = list_to_int a 1
                   int_b = list_to_int b 1
                   int_n = list_to_int rev_ns 1
                   rev_a = reverse a
                   rev_b = reverse b
                   d1 = int_n - int_a
                   d2 = int_b - int_n
