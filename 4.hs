import Data.Map hiding (map, filter, foldl)
import Data.Char (isLetter)
import Data.List (sortBy)
import Data.Ord
import Data.List.Split (splitOn)

orderByMostFrequentCharacter :: String -> String
orderByMostFrequentCharacter =
    map (fst) .
    sortBy (comparing (Down . snd)) . countOccurences

countOccurences :: String -> [(Char, Int)]
countOccurences x = toList $ fromListWith (+) [(c, 1) | c <- filter (isLetter) x]

createExpectedChecksum :: String -> String
createExpectedChecksum = take 5 . orderByMostFrequentCharacter

getFirstPartOfCode :: String -> String
getFirstPartOfCode = head . splitOn "["

getChecksumFromCode :: String -> String
getChecksumFromCode = init . (!! 1) . splitOn "["

getSectorId :: String -> Int
getSectorId x = read (head . splitOn "[" . last $ splitOn "-" x) :: Int

isRoomValid :: String -> Bool
isRoomValid x = createExpectedChecksum (getFirstPartOfCode x) == getChecksumFromCode x

testInput :: String
testInput = "aaaaa-bbb-z-y-x-123[abxyz]"

solution :: IO Int
solution = sum . map (\n -> if isRoomValid n then getSectorId n else 0)  <$> lines <$> readFile "4.input"
