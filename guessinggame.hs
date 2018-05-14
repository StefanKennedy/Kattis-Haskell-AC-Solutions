
main = do
  inp <- getContents
  putStrLn (sol inp)

sol = format . reverse . solver . parse . reverse . lines

parsestep::([String], Int) -> [(Bool, Int, Int)]
parsestep ([],_) = []
parsestep ((a:(g:rst)), c)
  | a == "too high" = ((False, (read g), c) : (parsestep (rst, c)))
  | a == "too low" = ((True, (read g), c) : (parsestep (rst, c)))
  | a == "right on" = ((True, -2, -1) : parsestep (rst, (read g)))

parse::[String] -> [(Bool, Int, Int)] -- > or <, guess, answer
parse (_:(_:(a:(b:rst)))) | b == "right on" = ((True, -2, -1): parsestep ((b:rst), (read a)))  
parse (_:(_:(a:rst))) = parsestep (rst, (read a))  

frmt::[(Bool, Int, Int)] -> String
frmt a = show (length a)

solve::[(Bool, Int, Int)] -> [(Bool, Int)]
solve [] = []
solve ((iq, g, c):rst)
  | (iq && (c > g)) || ((not iq) && (c < g)) = ((True, c):solve(rst))
  | otherwise = ((False, c):solve(rst))

solver::[(Bool, Int, Int)] -> [Bool]
solver a = squisher (solve a)

squisher::[(Bool, Int)]->[Bool]
squisher ((a,b):[]) = [a]
squisher ((_,-1):rst) = (True:(squisher rst))
squisher ((hv, he):((hv1, he1):rst)) 
  | he1 == -1 = (hv:(squisher rst))
  | otherwise = squisher ((hv&&hv1,he):rst)

format::[Bool] -> String
format [] = ""
format (a:rst) 
  | a == True  = "Stan may be honest\n"++format(rst)
  | a == False = "Stan is dishonest\n"++format(rst)

squish::[Bool] -> Bool
squish [] = True
squish (a:rst) = a && squish(rst)
