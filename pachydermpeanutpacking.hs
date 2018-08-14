import Data.Char

data Box = Box Float Float Float Float String
data Peanut = Peanut Float Float String

main = do
  contents <- getContents
  putStrLn (sol contents)

sol = format . parse

correctornot::(Box, Peanut) -> [String]
correctornot ((Box _ _ _ _ a), (Peanut _ _ b)) | a == b = [b, "correct"]
                                               | otherwise = [b, a]

inbox::(Box, Peanut) -> Bool
inbox ((Box x1 y1 x2 y2 _), (Peanut x y _)) = x >= x1 && x <= x2 && y >= y1 && y <= y2

ans::([Box], Peanut) -> [String]
ans ([], (Peanut _ _ typ)) = [typ, "floor"]
ans ((box:boxes), p) | inbox(box, p) = correctornot(box, p)
                     | otherwise = ans(boxes, p)

getans::([Box], [Peanut])-> [[String]]
getans (_, []) = []
getans (boxes, (p:peanuts)) = ((ans (boxes, p)):getans(boxes, peanuts))

readpeanut::[String] -> Peanut
readpeanut (a:(b:(c:[]))) = Peanut (read a :: Float) (read b :: Float) c

readpeanuts::(Int, [String], [[[String]]], [Box], [Peanut]) -> [[[String]]]
readpeanuts (i, (h:rst), tc, boxes, peanuts) | i == 0 = startboxes((h:rst), ((getans(boxes, peanuts)):tc))
                                             | otherwise = readpeanuts ((i-1), rst, tc, boxes, ((readpeanut (words h)):peanuts))

startpeanuts::([String], [[[String]]], [Box]) -> [[[String]]]
startpeanuts ((h:rst), tc, boxes) = readpeanuts ((read h), rst, tc, boxes, [])

readbox::[String] -> Box
readbox (a:(b:(c:(d:(e:[]))))) = Box (read a :: Float) (read b :: Float) (read c :: Float) (read d :: Float) e

readboxes::(Int, [String], [[[String]]], [Box]) -> [[[String]]]
readboxes (i, (h:rst), tc, boxes) | i == 0 = startpeanuts((h:rst), tc, boxes)
                                  | otherwise = readboxes ((i-1), rst, tc, ((readbox (words h)):boxes))

startboxes::([String], [[[String]]]) -> [[[String]]]
startboxes ((h:_), tc) | h == "0" = tc
startboxes (h:rst, tc) = readboxes((read h), rst, tc, [])

parse::String -> [[[String]]]
parse a = startboxes ((lines a), [])

format::[[[String]]] -> String
format (h:rst) | h == [] = format(rst)
format (h:[]) = formatt(h)
format (h:rst) = format(rst) ++ "\n\n" ++ formatt(h)

formatt::[[String]] -> String
formatt (g:[]) = formattt(g)
formatt (h:rst) = formatt(rst) ++ "\n" ++ formattt(h)

formattt::[String] -> String
formattt [] = "missing"
formattt (a:(b:[])) = a ++ " " ++ b
