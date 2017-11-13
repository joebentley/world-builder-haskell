import Text.Read (readMaybe)

main :: IO ()
main = do
    putStrLn "Please enter a number"
    a <- getLine
    case readMaybe a :: Maybe Double of
        Just x -> putStrLn ("The double of your number is " ++ show (2 * x))
        Nothing -> putStrLn "Could not read number"

main2 :: IO ()
main2 = do
    putStrLn "Please enter a number"
    a1 <- getLine
    putStrLn "Please enter another number"
    a2 <- getLine

    case (+) <$> (readMaybe a1) <*> (readMaybe a2) of
        Just x -> putStrLn ("The sum of your numbers is " ++ show x)
        Nothing -> putStrLn "Could not read number"