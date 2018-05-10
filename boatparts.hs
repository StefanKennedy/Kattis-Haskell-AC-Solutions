import qualified Data.Set as Set

main =do
  as<- getLine
  bs<- mapM (const getLine) [1.. sol as]
  putStrLn (ans ((reverse bs), (prt as)))

sol= right . both . toint . split
prt= left . both . toint . split

ans::([String], Int) -> String
ans (lst, a) | Set.size (Set.fromList lst) < a = "paradox avoided"
ans ((h:rst), a)
  | Set.size (Set.fromList rst) < a = show ((length rst) + 1)
  | otherwise = ans (rst, a)

split::String->[String]
split as=words as

toint::[String]->[Int]
toint as=map read as

both::[Int]->(Int, Int)
both a=(a !! 0, a !! 1)

left::(Int, Int)->Int
left (a, b) = a

right::(Int, Int)->Int
right (a, b) = b
