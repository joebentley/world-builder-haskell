{-# LANGUAGE InstanceSigs #-}

data Maybe' a = Nothing' | Just' a deriving Show

instance Functor Maybe' where
    fmap f (Just' a) = Just' (f a)
    fmap _ Nothing' = Nothing'

instance Applicative Maybe' where
    pure = Just'

    _ <*> Nothing' = Nothing'
    Nothing' <*> _ = Nothing'
    Just' f <*> Just' a = Just' $ f a

newtype ZipList a = ZipList { getZipList :: [a] } deriving Show

instance Functor ZipList where
    fmap f = ZipList . fmap f . getZipList

instance Applicative ZipList where
    pure :: a -> ZipList a
    pure a = ZipList $ repeat a   -- exercise

    (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)