import Interface
import World
import qualified Data.Map as Map

main :: IO ()
main = loop testState >> return ()

test1 :: IO ()
test1 = parse testState "@create room Jail Cell Abode" >>= putStrLn . show . world

test2 :: IO ()
test2 = parse testState "@create room Jail Cell Abode"
    >>= \st -> parse st "@set room 1 desc This is a horrid room"
    >>= putStrLn . show . world

testRoom :: Room
testRoom = Room
    { roomID = 0, name = "Jail Cell", attrs = Map.fromList [("desc", "Horrible jail cell")],
      exits = Map.fromList [("n", 1)] }

testRoom2 :: Room
testRoom2 = Room
    { roomID = 1, name = "Corridor", attrs = Map.fromList [("desc", "Dark corridor")],
      exits = Map.fromList [("s", 0)] }

testState :: State
testState = apply (addRoom testRoom2 . addRoom testRoom) $ emptyState