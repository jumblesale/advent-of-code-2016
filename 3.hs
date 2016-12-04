areSidesValid :: Int -> Int -> Int -> Bool
areSidesValid x y z = (sum [x, y, z]) > (2 * (maximum [x, y, z]))

-- apply list elements
areSidesValidFromList :: [Int] -> Bool
areSidesValidFromList (x:y:z:zs) = areSidesValid x y z

-- given an input like
--     4   21  894
--   419  794  987
-- turn it into
-- [[4, 21, 894], [419, 794, 987]]
parseInput :: String -> [[Int]]
parseInput x = map (map (\n -> read n :: Int)) $ map words $ lines x

solution = length . filter (==True) <$>
    map areSidesValidFromList <$>
    parseInput <$>
    readFile "3.input"
