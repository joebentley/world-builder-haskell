
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
                  attrs = Map.fromList [("desc", "Horrible jail cell")], exits = Map.empty }

testState :: State
testState = apply (addRoom testRoom) $ emptyState

parse :: State -> String -> IO State
parse state str
    | length str == 0   = return state
    | str == "q"        = exitSuccess
    | ws !! 0 == "look" =
        let r = getRoomByID (currentRoom state) $ wld in
            putStr (maybe "" (\x -> name x ++ "\n") r) >> return state
    | ws !! 0 == "@create" && ws !! 1 == "room" =
        let roomName = unwords $ drop 2 ws
            room = Room { roomID = getNextRoomID wld, name = roomName, attrs = Map.empty, exits = Map.empty } in
            return . State (currentRoom state) $ addRoom room wld
    | ws !! 0 == "@list" && ws !! 1 == "room" =
        (putStr . foldr (\x y -> x ++ "\n" ++ y) ""
                . map (\(_roomID, room) -> "(" ++ show _roomID ++ ": " ++ name room ++ ")") . Map.toList . rooms $ wld)
               >> return state
    | otherwise         = return state
    where
        ws = words str
        wld = world state

loop :: State -> IO State
loop s = putStr "> " >> getLine >>= parse s >>= loop

main :: IO ()
main = loop testState >> return ()

test1 :: IO ()
test1 = parse testState "@create room Jail Cell Abode" >>= putStrLn . show . world