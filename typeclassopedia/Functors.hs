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

data Tuple e a = Tuple e a deriving Show

instance Functor (Tuple e) where
    fmap f (Tuple e a) = Tuple e (f a)

data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

-- Tuple has kind * -> * -> * whereas Pair has kind * -> *
-- This makes it possible to define a Functor instance for the type Pair
-- which is not possible for Tuple (instead we have to implement an instance
-- for Tuple e). For the Tuple, the first element is left alone, whereas
-- in the Pair both elements are modified.

data ITree a = Leaf (Int -> a) | Node [ITree a]

instance Functor ITree where
    fmap :: (a -> b) -> ITree a -> ITree b
    fmap f (Leaf g) = Leaf $ f . g
    fmap f (Node x) = Node . map (fmap f) $ x

data Compose f g x = Compose { unComp :: f (g x) }

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f = Compose . fmap (fmap f) . unComp
