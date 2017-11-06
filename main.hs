
import System.Exit
import qualified Data.Map as Map
import World

data State = State { currentRoom :: Int, world :: World }

apply :: (World -> World) -> State -> State
apply f (State c w) = State c (f w)

emptyState :: State
emptyState = State 0 emptyWorld

testRoom :: Room
testRoom = Room { roomID = 0, name = "Jail Cell",
                  attrs = Map.fromList [("desc", "Horrible jail cell")], exits = Map.empty}

testState :: State
testState = apply (addRoom testRoom) $ emptyState

parse :: State -> String -> IO State
parse state str
    | length str == 0   = return state
    | str == "q"        = exitSuccess
    | ws !! 0 == "look" =
        let r = getRoomByID (currentRoom state) $ world state in
            putStr (maybe "" (\x -> name x ++ "\n") r) >> return state
    | otherwise         = return state
    where ws = words str

loop :: State -> IO State
loop s = putStr "> " >> getLine >>= parse s >>= loop

main :: IO ()
main = loop testState >> return ()