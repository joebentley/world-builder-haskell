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