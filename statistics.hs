find_cons :: Char -> String -> Int
find_cons _ "" = 1
find_cons c (g:goals) = if (c == g) then find_cons c goals +1
                                 else 1

find_turns :: String -> Int -> Int -> Int
find_turns "" _ _ = 0
find_turns (g:goals) s1 s2
            | ((g == 'a') && (s1-t < s2) && (s1+t > s2)) = t
            | ((g == 'b') && (s2-t < s1) && (s2+t > s1)) = t
            | otherwise = 0
            where t = find_cons g goals

calculator :: String -> ((Int, Int), Int, Int) -> ((Int, Int), Int, Int)
calculator (g:goals) ((s1, s2), ties, turns) 
            | (g == 'a' && s1+1 == s2) = ((s1+1, s2), ties+1, turns)
            | (g == 'a') = ((s1+1, s2), ties, max turns (find_turns (g:goals) (s1+1) s2) )
            | (g == 'b' && s1 == s2+1) = ((s1, s2+1), ties+1, turns)
            | otherwise = ((s1, s2+1), ties, max turns (find_turns (g:goals) s1 (s2+1) ) )

procedure :: String -> ((Int, Int), Int, Int)
procedure "" = ((0, 0), 1, 0)
procedure (g:goals) = calculator (g:goals) (procedure goals)

statistics :: String -> ((Int, Int), Int, Int)
statistics(goals) = procedure(reverse(goals))
