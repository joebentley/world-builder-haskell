module World where

import qualified Data.Map as Map

data Room = Room {
    roomID :: Int,
    name :: String,
    attrs :: Map.Map String String,
    exits :: Map.Map String Int
} deriving Show

data World = World { rooms :: Map.Map Int Room } deriving Show

emptyWorld :: World
emptyWorld = World { rooms = Map.empty }

iterateUntilNoKey :: Int -> Map.Map Int a -> Int
iterateUntilNoKey i m = if Map.member i m then iterateUntilNoKey (i + 1) m else i

getFreeKey :: Map.Map Int a -> Int
getFreeKey m = iterateUntilNoKey (Map.size m) m

addRoom :: Room -> World -> World
addRoom r (World m) = World $ Map.insert (getFreeKey m) r m

getRoomByID :: Int -> World -> Maybe Room
getRoomByID i w = Map.lookup i $ rooms w

getNextRoomID :: World -> Int
getNextRoomID (World m) = getFreeKey m

changeRoomAttribute :: World -> Int -> String -> String -> World
changeRoomAttribute w _roomID key value =
    let room = Map.lookup _roomID . rooms $ w in
        maybe w (\r -> World . Map.adjust (\_ -> r { attrs = Map.insert key value . attrs $ r}) _roomID . rooms $ w) room

getRoomAttribute :: World -> Int -> String -> Maybe String
getRoomAttribute w _roomID key = (Map.lookup _roomID . rooms $ w) >>= Map.lookup key . attrs