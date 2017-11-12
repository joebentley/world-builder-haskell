{-# LANGUAGE OverloadedStrings #-}

module World where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Data.Monoid

data Room = Room {
    roomID  :: Index,
    name    :: Text,
    attrs   :: Map.Map Text Text,
    exits   :: Map.Map Text Index
} deriving Show

-- decode "{\"roomID\" : 4, \"name\" : \"Cell\", \"attrs\": {\"desc\": \"empty\"}, \"exits\": {\"n\": 4}}" :: Maybe Room
instance FromJSON Room where
    parseJSON = withObject "Room" $ \v -> Room
        <$> (v .: "roomID")
        <*> (v .: "name")
        <*> (v .: "attrs")
        <*> (v .: "exits")

instance ToJSON Room where
    toJSON (Room _roomID _name _attrs _exits) =
        object ["roomID" .= _roomID, "name" .= _name, "attrs" .= _attrs, "exits" .= _exits]
    
    toEncoding (Room _roomID _name _attrs _exits) =
        pairs ("roomID" .= _roomID <> "name" .= _name <> "attrs" .= _attrs <> "exits" .= _exits)

data World = World { rooms :: Map.Map Index Room } deriving Show

instance FromJSON World where
    parseJSON = withObject "World" $ \v -> World <$> (v .: "rooms")

instance ToJSON World where
    toJSON (World _rooms) = object ["rooms" .= _rooms]
    toEncoding (World _rooms) = pairs ("rooms" .= _rooms)

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

changeRoomAttribute :: World -> Index -> Text -> Text -> World
changeRoomAttribute w _roomID key value =
    let room = Map.lookup _roomID . rooms $ w in
        maybe w (\r -> World . Map.adjust (\_ -> r { attrs = Map.insert key value . attrs $ r}) _roomID . rooms $ w) room

getRoomAttribute :: World -> Index -> Text -> Maybe Text
getRoomAttribute w _roomID key = (Map.lookup _roomID . rooms $ w) >>= Map.lookup key . attrs
