split :: [(String, String)] -> ([String], [String])
split [] = ([], [])
split ((str1, str2):rest) = ([str1]++rest1, [str2]++rest2)
                            where (rest1, rest2) = split rest

merge_strings :: [String] -> String
merge_strings [] = []
merge_strings (str:strs) = str++(merge_strings strs)

pop_ith :: [(String, String)] -> Int -> (String, String)
pop_ith strs 1 = head strs
pop_ith (e:rest) i = pop_ith rest (i-1)

build :: [Int] -> [(String, String)] -> [(String, String)]
build [] _ = []
build (idx:idxs) strs = [(pop_ith strs idx)]++(build idxs strs)

valid :: [Int] -> [(String, String)] -> Bool
valid idxs strs = if (str1 == str2) then True
                  else False
                  where str1 = merge_strings strs1
                        str2 = merge_strings strs2
                        (strs1, strs2) = split cleaned
                        cleaned = build idxs strs

next :: [Int] -> Int -> [Int]
next (n:ns) max_num = if n < max_num then ([n+1]++ns)
                else if ns == [] && n == max_num then []
                else [1]++(next ns max_num)

find_solution :: [(String, String)] -> [Int] -> [Int]
find_solution strs sol = if sol == [] then [] 
                           else if valid sol strs then sol
                           else find_solution strs (next sol (length strs))

init_list :: Int -> [Int]
init_list 0 = []
init_list k = [1]++(init_list (k-1))

search :: [(String, String)] -> Int -> [Int]
search strs k = if sol == [] then search strs (k+1)
                else sol
                where sol = find_solution strs (init_list k)

superstring :: [(String, String)] -> [Int]
superstring strs = search strs 1
