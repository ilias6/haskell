shuffle :: [Int]->Int->[Int]
shuffle is 0 = is
shuffle (i:is) n = shuffle is (n-1)++[i]
