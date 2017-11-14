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
    a1 <- readMaybe <$> getLine
    putStrLn "Please enter another number"
    a2 <- readMaybe <$> getLine

    case (+) <$> a1 <*> a2 of
        Just x -> putStrLn ("The sum of your numbers is " ++ show x)
        Nothing -> putStrLn "Could not read number"

interactiveConcatenating :: IO ()
interactiveConcatenating = do
    putStrLn "Choose two strings:"
    s <- (++) <$> getLine <*> getLine
    putStrLn "Let's concatenate them:" *> putStrLn s