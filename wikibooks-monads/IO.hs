
import Control.Monad

printList :: Show a => [a] -> IO ()
printList = mapM_ (putStrLn . show)

generation :: a -> [a]
generation = replicate 3

bunnyGenerations :: Int -> [String]
bunnyGenerations n = foldr (<=<) return (replicate n generation) "bunny"