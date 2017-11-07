{-# LANGUAGE InstanceSigs #-}

data Either' e a = Left' e | Right' a deriving Show

instance Functor (Either' e) where
    fmap :: (a -> b) -> Either' e a -> Either' e b
    fmap _ (Left' e) = Left' e
    fmap g (Right' a) = Right' (g a)

testEitherFunctor :: IO ()
testEitherFunctor = (putStrLn . show . fmap (+1) $ Left' 4) >>
                    (putStrLn . (show :: Either' Int Int -> String) . fmap (+1) $ Right' 4)

data Func e a = Func e a deriving Show

instance Functor (Func e) where
    fmap g (Func e a) = Func e (g a)

testFuncFunctor :: IO ()
testFuncFunctor = putStrLn . show . fmap (+1) $ Func 5 1