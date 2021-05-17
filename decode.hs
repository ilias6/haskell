delete :: Char -> String -> String
delete _ "" = ""
delete c1 (c2:str) = if (c1 == c2) then str
                     else [c2]++delete c1 str

get_n :: String -> Int -> (String, String)
get_n "" _ = ("", "")
get_n sms 0 = ("", sms)
get_n (c:sms) n = ([c]++str1, str2)
                  where (str1, str2) = get_n sms (n-1)

solution :: String -> Int -> String
solution "" _ = ""
solution sms i = if part1 == part2 then part1++(solution rest2 (div (length rest2) 2))
                     else solution sms (i-1)
                     where (part2, rest2) = get_n rest1 i
                           (part1, rest1) = get_n sms i

decodesms :: String -> String
decodesms sms = solution sms (div (length sms) 2) 
