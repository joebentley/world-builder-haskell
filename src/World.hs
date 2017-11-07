module World where

import qualified Data.Map as Map

data Room = Room {
    roomID :: Index,
    name :: String,
    attrs :: Map.Map String String,
    exits :: Map.Map String Index
} deriving Show

data World = World { rooms :: Map.Map Index Room } deriving Show

type Index = Int

emptyWorld :: World
emptyWorld = World { rooms = Map.empty }

iterateUntilNoKey :: Index -> Map.Map Index a -> Index
iterateUntilNoKey i m = if Map.member i m then iterateUntilNoKey (i + 1) m else i

getFreeKey :: Map.Map Index a -> Index
getFreeKey m = iterateUntilNoKey (Map.size m) m

addRoom :: Room -> World -> World
addRoom r (World m) = World $ Map.insert (getFreeKey m) r m

getRoomByID :: Index -> World -> Maybe Room
getRoomByID i w = Map.lookup i $ rooms w

getNextRoomID :: World -> Index
getNextRoomID (World m) = getFreeKey m

changeRoomAttribute :: World -> Index -> String -> String -> World
changeRoomAttribute w _roomID key value =
    let room = Map.lookup _roomID . rooms $ w in
        maybe w (\r -> World . Map.adjust (\_ -> r { attrs = Map.insert key value . attrs $ r}) _roomID . rooms $ w) room

getRoomAttribute :: World -> Index -> String -> Maybe String
getRoomAttribute w _roomID key = (Map.lookup _roomID . rooms $ w) >>= Map.lookup key . attrs