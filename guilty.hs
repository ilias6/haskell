make_table :: [Int] -> Int -> [(Int, Int)]
make_table [] _ = []
make_table (v:votes) id = [(v, id)]++(make_table votes (id+1))

delete_invalid :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
delete_invalid [] _ _ = []
delete_invalid ((vote1, id1):rest) id2 vote2 = if ((vote1 == id2) || (vote2 == id1)) then delete_invalid rest id2 vote2
                                      else [(vote1, id1)]++(delete_invalid rest id2 vote2)

max_guilty :: [(Int, Int)] -> [(Int, Int)]
max_guilty [] = []
max_guilty ((vote,  id):rest) = [(vote, id)]++(max_guilty (delete_invalid rest id vote))

member :: [(Int, Int)] -> Int -> Bool
member [] _ = False
member ((_, id1):rest) id2 = if (id1 == id2) then True
                             else member rest id2

find_max :: [(Int, Int)] -> Int -> Int -> Int
find_max [_] _ _= 1
find_max table id n = trace (show table) $ if (last_id < (id-1)) || ((length table) <= n) || (not (member table id)) then 0
                      else max (max n1 n) (max n2 n3)
                                  where (last_id, _) = last table
                                        n1 = length (max_guilty (table))
                                        n2 = find_max (delete_invalid table (-1) id) (id+1) n1
                                        n3 = find_max table (id+1) n1

guilty :: [Int] -> Int
guilty (votes) = find_max (make_table votes 1) 1 0
