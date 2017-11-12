{-# LANGUAGE OverloadedStrings #-}

module Interface where

import System.Exit
import Data.Maybe
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.IO
import Data.Maybe
-- import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import World

-- Helper functions
displayRoomList :: World -> Text
displayRoomList w = foldr (\x y -> mconcat [x, "\n", x]) ""
    . map (\(_roomID, room) -> mconcat ["(", (T.pack . show $ _roomID), ": ", name room, ")"]) . Map.toList . rooms $ w


data State = State { currentRoom :: Index, world :: World }

apply :: (World -> World) -> State -> State
apply f (State c w) = State c (f w)

emptyState :: State
emptyState = State 0 emptyWorld

parse :: State -> Text -> IO State
parse state str
    | T.length str == 0 = return state
    | str == "q"        = exitSuccess
    | ws !! 0 == "look" =
        let r = getRoomByID (currentRoom state) $ wld in
            TIO.putStrLn (maybe "" (\x -> name x) r) >>
            TIO.putStrLn (maybe "" (\x -> maybe "" (\y -> T.append "\n" y) . Map.lookup "desc" . attrs $ x) r) >> return state
    | ws !! 0 == "@create" && ws !! 1 == "room" =
        let roomName = T.unwords $ drop 2 ws
            room = Room { roomID = getNextRoomID wld, name = roomName, attrs = Map.empty, exits = Map.empty } in
            return $ state { world = addRoom room wld }
    | ws !! 0 == "@list" && ws !! 1 == "room" =
        (TIO.putStr . displayRoomList $ wld) >> return state
    | ws !! 0 == "@set" && ws !! 1 == "room" =
        let _roomID = ws !! 2; key = ws !! 3; value = T.unwords . drop 4 $ ws
            parsedID = decimal $ _roomID in
            return . either (\_ -> state)
                            (\(_parsedID, _) -> state { world = changeRoomAttribute wld _parsedID key value }) $ parsedID
    | ws !! 0 `elem` ["n", "s", "e", "w", "ne", "nw", "se", "sw"] =
        let room = fromJust . getRoomByID (currentRoom state) $ wld in -- assume that current room is valid
            (\s -> parse s "look") $ state { currentRoom = maybe (currentRoom state) id . Map.lookup (ws !! 0) $ exits room}
    | ws !! 0 == "@save" =
        let filepath = ws !! 1 in
            do
                h <- openFile (T.unpack filepath) ReadWriteMode
                B.hPutStr h (BL.toStrict . encodePretty $ wld);
                hClose h;
                return state
    | ws !! 0 == "@load" =
        let filepath = ws !! 1 in
            do
                h <- openFile (T.unpack filepath) ReadWriteMode
                s <- B.hGetContents h
                hClose h;
                return . State 0 . fromJust . decode . BL.fromStrict $ s
    | otherwise = return state
    where
        ws = T.words str
        wld = world state

loop :: State -> IO State
loop state = do
    putStr "> ";
    hFlush stdout;
    l <- TIO.getLine;
    newState <- parse state l;
    loop newState