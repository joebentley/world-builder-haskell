import World
import Interface

import qualified Data.Map as Map

testGetFreeKey :: Map.Map Int Char -> Int -> Bool
testGetFreeKey m i = getFreeKey m == i

testGetFreeKeyCases :: Map.Map String Bool
testGetFreeKeyCases = Map.fromList
    [
        ("testGetFreeKey1", testGetFreeKey (Map.fromList [(1, 'a'), (0, 'b'), (2, 'c'), (4, 'd')]) 5),
        ("testGetFreeKey2", testGetFreeKey Map.empty 0)
    ]

-- testCreateRoom :: Map.Map String Bool
-- testCreateRoom = Map.fromList
--     [
--         ("testCreateRoom1", )
--     ]

testAllCases :: Map.Map String Bool
testAllCases = Map.union testGetFreeKeyCases Map.empty

main :: IO ()
main = putStr . Map.foldrWithKey (\k x y -> k ++ ": " ++ x ++ "\n" ++ y) "" .
    Map.map (\x -> if x then "Passed" else "Failed") $ testAllCases