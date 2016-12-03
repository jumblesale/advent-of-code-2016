import Prelude hiding (Left, Right)

-- represents the keypad
-- 1 2 3
-- 4 5 6
-- 7 8 9
-- empty allows us to use fromEnum to go 1 -> One
data Key = 
    Empty |
    One   |
    Two   |
    Three |
    Four  |
    Five  |
    Six   |
    Seven |
    Eight |
    Nine
    deriving (Show, Eq, Enum)

-- (0,0) is 1, (3,3) is 9
type Coord = (Int, Int)

data Direction = Up | Down | Left | Right
    deriving (Show)

-- we're either going to move to a different button
-- or take a look at what button we're on
data Action = Move Direction | Peek
    deriving (Show)

-- translate a location on the keypad to a key
coordToKey :: Coord -> Key
coordToKey (0,0) = One
coordToKey (1,0) = Two
coordToKey (2,0) = Three
coordToKey (0,1) = Four
coordToKey (1,1) = Five
coordToKey (2,1) = Six
coordToKey (0,2) = Seven
coordToKey (1,2) = Eight
coordToKey (2,2) = Nine

-- doesn't care if movement goes out of bounds
moveUnsafe :: Coord -> Direction -> Coord
moveUnsafe (x, y) Up = (x, y-1)
moveUnsafe (x, y) Down = (x, y+1)
moveUnsafe (x, y) Left = (x-1, y)
moveUnsafe (x, y) Right = (x+1, y)

-- do the move but check bounds
move :: Coord -> Direction -> Coord
move (x, y) d
    | x' < 0 || x' > 2 = (x, y)
    | y' < 0 || y' > 2 = (x, y)
    | otherwise = (x', y')
    where (x', y') = moveUnsafe (x, y) d

-- map UDLR to Up | Down | Left | Right
commandToAction :: Char -> Action
commandToAction 'U' = (Move Up)
commandToAction 'D' = (Move Down)
commandToAction 'L' = (Move Left)
commandToAction 'R' = (Move Right)
commandToAction _ = error "not a valid direction"

-- given a string like UURD
-- produce [(Movement Up), (Movement Up), (Movement Right), (Movement Down)]
stringToActions :: String -> [Action]
stringToActions = map commandToAction

-- given a string like "UD\nRR"
-- produce [(Movement Up), (Movement Down), Peek, (Movement Right), (Movement Right)]
-- inputToActions :: [String] -> [Action]
inputToActions [] = []
inputToActions (x:xs) = (stringToActions x) ++ Peek : (inputToActions xs)

actions :: [Action]
actions = inputToActions $ lines input

execute :: Coord -> [Action] -> [Int] -> [Int]
execute c ((Move d):xs) keys = execute (move c d) xs keys
execute c ((Peek):xs) keys = execute c xs (keys ++ [(fromEnum $ coordToKey c)])
execute _ [] keys = keys

solution :: [Int]
solution = execute (1,1) actions []

test :: [Int]
test = execute (1,1) (inputToActions $ lines testInput) []

testInput = "ULL\n\
\RRDDD\n\
\LURDL\n\
\UUUUD"

input :: String
input = "UULLULLUULLLURDLDUURRDRRLDURDULLRURDUDULLLUULURURLRDRRRRULDRUULLLLUUDURDULDRRDRUDLRRLDLUDLDDRURURUURRRDDDLLRUDURDULUULLRRULLRULDUDRDRLDLURURUDDUDLURUDUDURLURURRURLUDDRURRDLUURLLRURRDUDLULULUDULDLLRRRDLRDLDUDRDDDRRUURRRRRUURRDRRDLURDRRURDLLUULULLRURDLDDDRRLLRRUURULURUUDDLRRUDDRURUUDLRLRDLRURRRDULLDLRUDDUULRDULURUURDULUDLLRRLDDLRDLRUDRLDDRLRRRDURDULLRRRDRRLUURURDRRDRRLDLUDURURLDUURDRUDRDDRLDRRLDLURURULLUURUDUUDLRLL\n\
\LLLULLULDDULRLLURLLLRUUDDLRUULRLULLDLLRRDRLRLRLLDRUUURULDRDDLUDLLDUDULLLRLULLLRULDRDRUDLLRLRLLUDULRRRLDRUULDDULLDULULLUDUDLDRDURDLDLLDUDRRRDLUURRUURULLURLDURLRRLLDDUUULDRLUUDUDLURLULUDURRDRLLDDDDDRRULLRLDULULDDRUURRDLUDDDUDURDDRDRULULLLLUURDURUUUULUDLRURRULRDDRURURLLRLUUDUUURDLLDDLUDRLLLUDLLLLULRLURDRRRDUUDLLDLDDDURRDDRURUURDDRURRLDDDURDLLUURUUULRLUURRUDRLLDLURDUDRLULDLRLULULUDDLRDUDRUDLUULUULDURDRRRRLRULLUDRDDRDLDUDRDRRLDLLLLUDDLRULDLLDDUULDDRRULRRUURUDRDURLLLDDUUDRUUDLULLDR\n\
\UDUUULLDDDDLUDLDULRLRDLULLDDRULDURRLURRUDLRRUDURRDUDRRRUULRLLRLUDLDRRDUURDDRDRDUUUDUDLDLLRRLUURLUUUDDDUURLULURRLURRRDRDURURUDRLRUURUDRUDDDRDRDLDRDURDLDRRDUUDLLURLDDURRRLULDRDRLLRLLLRURLDURDRLDRUURRLDLDRLDDDRLDLRLDURURLLLLDDRDUDLRULULLRDDLLUDRDRRLUUULDRLDURURDUDURLLDRRDUULDUUDLLDDRUUULRRULDDUDRDRLRULUUDUURULLDLLURLRRLDDDLLDRRDDRLDDLURRUDURULUDLLLDUDDLDLDLRUDUDRDUDDLDDLDULURDDUDRRUUURLDUURULLRLULUURLLLLDUUDURUUDUULULDRULRLRDULDLLURDLRUUUDDURLLLLDUDRLUUDUDRRURURRDRDDRULDLRLURDLLRRDRUUUURLDRURDUUDLDURUDDLRDDDDURRLRLUDRRDDURDDRLDDLLRR\n\
\ULDRUDURUDULLUDUDURLDLLRRULRRULRUDLULLLDRULLDURUULDDURDUUDLRDRUDUDDLDRDLUULRRDLRUULULUUUDUUDDRDRLLULLRRDLRRLUDRLULLUUUUURRDURLLRURRULLLRLURRULRDUURRLDDRRDRLULDDRRDRLULLRDLRRURUDURULRLUDRUDLUDDDUDUDDUDLLRDLLDRURULUDRLRRULRDDDDDRLDLRRLUUDLUURRDURRDLDLDUDRLULLULRLDRDUDLRULLULLRLDDRURLLLRLDDDLLLRURDDDLLUDLDLRLUULLLRULDRRDUDLRRDDULRLLDUURLLLLLDRULDRLLLUURDURRULURLDDLRRUDULUURRLULRDRDDLULULRRURLDLRRRUDURURDURDULURULLRLDD\n\
\DURLRRRDRULDLULUDULUURURRLULUDLURURDDURULLRRUUDLRURLDLRUDULDLLRRULLLLRRLRUULDLDLLRDUDLLRLULRLLUUULULRDLDLRRURLUDDRRLUUDDRRUDDRRURLRRULLDDULLLURRULUDLRRRURRULRLLLRULLRRURDRLURULLDULRLLLULLRLRLLLDRRRRDDDDDDULUUDUDULRURDRUDRLUULURDURLURRDRRRRDRRLLLLUDLRRDURURLLULUDDLRLRLRRUURLLURLDUULLRRDURRULRULURLLLRLUURRULLLURDDDRURDUDDULLRULUUUDDRURUUDUURURRDRURDUDRLLRRULURUDLDURLDLRRRRLLUURRLULDDDUUUURUULDLDRLDUDULDRRULDRDULURRUURDU"