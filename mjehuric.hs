
main = do
  a <- getLine
  putStrLn (sol a)

sol = format . startloop . parse

parse::String -> [Int]
parse a = map read (words a)

putt_head::(Int, [Int]) -> [Int]
putt_head (h, y) = (h:y)

put_head::(Int, [[Int]]) -> [[Int]]
put_head (h, []) = []
put_head (h, (y:z)) = (putt_head (h,y):put_head (h,z))

step::[Int] -> [[Int]]
step (h:[]) = [[h]]
step (h:(h1:rst)) | h > h1 = ((h1:(h:rst)):(put_head (h1, (step (h:rst)))))
                  | otherwise = put_head(h, (step (h1:rst)))

get_steps::[Int] -> [[Int]]
get_steps stat = reverse (step stat)

eqs::([Int], [Int])->Bool
eqs ([], []) = True
eqs ((a:a1), (b:b1)) | a == b = eqs (a1, b1)
                     | otherwise = False

remove_dup::[[Int]] -> [[Int]]
remove_dup (a:[]) = (a:[])
remove_dup (a:(b:c)) | eqs (a, b) = (remove_dup (b:c))
                     | otherwise = (a:(remove_dup (b:c)))

format_inner::[Int] -> String
format_inner [] = ""
format_inner (a:b) = (show a)++" "++(format_inner b)

format::[[Int]] -> String
format [] = ""
format (a:b) = (format_inner a)++"\n"++(format b)

sorted::[Int] -> Bool
sorted (a:[]) = True
sorted (a:(b:z)) | b < a = False 
                 | otherwise = sorted (b:z)

loop::[[Int]] -> [[Int]]
loop (h:z) | sorted h = (h:z)
           | otherwise = loop ((get_steps h) ++ z)

startloop::[Int] -> [[Int]]
startloop h = reverse (pop (loop [h]))

pop::[[Int]]->[[Int]]
pop (a:b) = b

removefirst::([[Int]], [Int]) -> [[Int]]
removefirst ((a:a1), b) | eqs (a, b) = a1
                        | otherwise = (a:a1)
