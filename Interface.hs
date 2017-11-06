module Interface where

import System.Exit
import qualified Data.Map as Map
import World

-- Helper functions
displayRoomList :: World -> String
displayRoomList w = foldr (\x y -> x ++ "\n" ++ y) ""
    . map (\(_roomID, room) -> "(" ++ show _roomID ++ ": " ++ name room ++ ")") . Map.toList . rooms $ w


data State = State { currentRoom :: Int, world :: World }

apply :: (World -> World) -> State -> State
apply f (State c w) = State c (f w)

emptyState :: State
emptyState = State 0 emptyWorld

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
            return $ state { world = addRoom room wld }
    | ws !! 0 == "@list" && ws !! 1 == "room" =
        (putStr . displayRoomList $ wld) >> return state
    | ws !! 0 == "@set" && ws !! 1 == "room" =
        let _roomID = ws !! 2; key = ws !! 3; value = unwords . drop 4 $ ws in
            return $ state { world = changeRoomAttribute wld (read _roomID) key value }
    | otherwise         = return state
    where
        ws = words str
        wld = world state

loop :: State -> IO State
loop s = putStr "> " >> getLine >>= parse s >>= loop