{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (liftA2)

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

sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL = foldr (\x ys -> liftA2 (:) x ys) $ pure []

class Functor f => Monoidal f where
    unit :: f ()
    (****) :: f a -> f b -> f (a,b)

class Monoidal f => Applicative' f where
    -- pure' :: a -> f a
    -- pure' a = 

    (<**>) :: f (a -> b) -> f a -> f b
    (<**>) g x = fmap (\x -> fst x $ snd x) . (g **** x)