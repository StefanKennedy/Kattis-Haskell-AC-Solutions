import Data.List

main = do
  getLine
  let allscale = ["", "A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"]
  song <- getLine
  putStrLn (solve (song, allscale))

notes::String -> [String]
notes all= words all

solve::(String, [String]) -> String
solve (song, allscale) =
  result (unwords (clear (search ((words song), (allscales allscale)))))

scale::[String] -> [String]
scale lst = [(lst!!0),(lst!!2),(lst!!4),(lst!!5),(lst!!7),(lst!!9),(lst!!11),(lst!!12)]

allscales::[String] -> [[String]]
allscales lst | length lst == 13 = []
allscales (h:rst) = ((scale rst): allscales(rst))

containsall::([String], [String])->String
containsall (tofind, tosearch)
  | length (intersect tofind tosearch) == length tofind = tosearch!!0
  | otherwise = ""

search::([String], [[String]])->[String]
search (song, []) = []
search (song, (h:scales)) =
  ((containsall (song, h)):(search(song,scales)))

clear::[String]->[String]
clear [] = []
clear (h:rst) | h == "" = clear(rst)
              | otherwise = (h:clear(rst))

result::String -> String
result a | a == "" = "none"
         | otherwise = a
