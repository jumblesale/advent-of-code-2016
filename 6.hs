import Data.Map as Map hiding (map)
import Data.List (transpose, sortBy)
import Data.Ord (comparing)

getMostOccuringCharacter :: String -> Char
getMostOccuringCharacter = 
    -- the sort will reverse the order we want to just reverse that
    -- then take the first element of the resulting list!
    -- for part 2 just remove the reverse - this could be 
    -- a parameter to the function? somehow?
    head . reverse
    -- take the first element of the tuple (the character)
    map (fst) .
    -- sort the (character, count) pairs by comparing the second element
    -- of the tuple
    sortBy (comparing snd) . countOccurences

countOccurences :: String -> [(Char, Int)]
countOccurences x = toList $ Map.fromListWith (+) [(c, 1) | c <- x]

stringToColumns :: String -> [String]
stringToColumns = transpose . lines

testInput :: String
testInput = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"

getMostOccuringCharacterInEachColumn :: String -> String
getMostOccuringCharacterInEachColumn = map (getMostOccuringCharacter) . stringToColumns

testSolution :: String
testSolution = getMostOccuringCharacterInEachColumn testInput

solution :: IO String
solution = getMostOccuringCharacterInEachColumn <$> readFile "6.input"
