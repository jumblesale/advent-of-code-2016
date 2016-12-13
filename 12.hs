import Data.Char (toUpper)
import Debug.Trace

-- the available commands
data Command = Cpy IdentifierOrValue Identifier
             | Inc Identifier
             | Dec Identifier
             | Jnz IdentifierOrValue Int
    deriving (Show)

-- the copy operation works on a register or a value
-- also jnz can take either
data IdentifierOrValue = Id Identifier | Value Int
    deriving (Show)

-- the four registers, A B C & D
data Identifier = A | B | C | D
    deriving (Read, Show)

-- a register is an identifier and its value
data Register = Register { _id    :: Identifier
                         , _value :: Int
                         }
    deriving (Show)

-- a program counter to show what instruction we are on
type PC = Int

-- the state of the process is four registers and a program counter
data State = State { _a  :: Register
                   , _b  :: Register
                   , _c  :: Register
                   , _d  :: Register
                   , _pc :: PC
                   }
   deriving (Show)

-- translate an Identifier (B) to a State accessor (_b)
identifierToStateAccessor :: Identifier -> (State -> Register)
identifierToStateAccessor A = _a
identifierToStateAccessor B = _b
identifierToStateAccessor C = _c
identifierToStateAccessor D = _d

-- the initial state of the processor
initialState :: State
initialState = State (Register A 0) (Register B 0) (Register C 0) (Register D 0) 0

-- the initial state of the processor for question 2
initialState2 :: State
initialState2 = State (Register A 0) (Register B 0) (Register C 1) (Register D 0) 0

-- a state with all different values for testing
testState :: State
testState = State (Register A 1) (Register B 2) (Register C 3) (Register D 4) 13

-- replace a register with a new value and increase the program counter
updateState :: State -> Register -> State
updateState (State a b c d pc) x@(Register A _) = State x b c d (pc + 1)
updateState (State a b c d pc) x@(Register B _) = State a x c d (pc + 1)
updateState (State a b c d pc) x@(Register C _) = State a b x d (pc + 1)
updateState (State a b c d pc) x@(Register D _) = State a b c x (pc + 1)

-- update the program counter without modifiying a register
updateProgramCounter :: State -> Int -> State
updateProgramCounter (State a b c d pc) v = State a b c d (pc + v)

-- perform some operation on a register
operateOnRegister :: State -> (Int -> Int) -> Identifier -> State
operateOnRegister s f i = updateState s (Register i ((f) value))
    where existingRegister = (identifierToStateAccessor i s)
          value = (_value existingRegister)

inc :: State -> Identifier -> State
inc s i = operateOnRegister s (+1) i

dec :: State -> Identifier -> State
dec s i = operateOnRegister s (subtract 1) i

cpy :: State -> IdentifierOrValue -> Identifier -> State
cpy s (Id i) target  = updateState s (Register target valueFromExistingRegister)
    where valueFromExistingRegister = _value (identifierToStateAccessor i s)
cpy s (Value v) target = updateState s (Register target v)

-- if the register is NOT 0, increment the program counter by the specified amount
jnz :: State -> IdentifierOrValue -> Int -> State
jnz s (Id i) v = jnz s (Value valueFromRegister) v
    where valueFromRegister = _value (identifierToStateAccessor i s)
jnz s (Value i) v = if i /= 0
    then updateProgramCounter s v
    else updateProgramCounter s 1

-- create a new state by executing the Command
execute :: State -> Command -> State
execute s (Inc i) = inc s i
execute s (Dec i) = dec s i
execute s (Cpy iov i) = cpy s iov i
execute s (Jnz i v) = jnz s i v

-- given a string like "c" or "a", convert it to an Identifier, C or A
toIdentifier :: String -> Identifier
toIdentifier x = read (map toUpper x) :: Identifier


-- given an input like "23" or "c", convert it to a Value 23 or an Id c
toIdentifierOrValue :: String -> IdentifierOrValue
toIdentifierOrValue x = if isIdentifier x
                        then (Id (toIdentifier x))
                        else (Value (read x :: Int))
    where isIdentifier y = (head y) `elem` "abcd"

-- convert a string representing a command (cpy 26 d) to a Command
-- (Cpy 26 D) note this is unsafe as heck and also a bit of a mess
parseCommand :: String -> Command
parseCommand s
    | cmd == "inc" = (Inc $ toIdentifier (parts !! 1))
    | cmd == "dec" = (Dec $ toIdentifier (parts !! 1))
    | cmd == "cpy" = (Cpy (toIdentifierOrValue (parts !! 1)) (toIdentifier (parts !! 2)))
    | cmd == "jnz" = (Jnz (toIdentifierOrValue (parts !! 1)) (toValue (parts !! 2)))
    | otherwise = error $ "command " ++ cmd ++ " not recognised"
    where 
        parts = words s
        cmd = parts !! 0
        toValue v = read v :: Int

executeCommands :: State -> Int -> [Command] -> State
executeCommands s i x =
    if pc >= (length x) then s
    else 
        --trace (show pc) $ trace (show s) $ trace (show currentInstruction) $ 
        executeCommands s' (i + 1) x
    where pc = _pc s
          s' = execute s currentInstruction
          currentInstruction = (x !! pc)

parseInput :: IO [Command]
parseInput = map parseCommand <$> lines <$> readFile "12.input"

solution :: IO State
solution = executeCommands initialState 0 <$> parseInput

solution2 :: IO State
solution2 = executeCommands initialState2 0 <$> parseInput