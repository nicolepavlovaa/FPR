module Upr where

    safeDiv::Int->Int->Maybe(Int, Int)
    safeDiv _ 0 = Nothing
    safeDiv a b = Just((a `quot` b), (a `rem` b))

    find :: (a -> Bool) -> [a] -> Maybe a
    find f l 
      | (length p /= 0) = Just(p !! 0)
      | otherwise = Nothing
      where p = (filter f l)

    stripPrefix :: String -> String -> Maybe String
    stripPrefix [] w = Just(w)
    stripPrefix (x:xs) (y:ys) 
      | x == y = stripPrefix xs ys
      | x /= y = Nothing
      | otherwise = Just(ys)

    lookupp :: Eq k => k -> [(k, v)] -> Maybe v
    lookupp key l 
      | length p /= 0 = Just(snd (p !! 0))
      | otherwise = Nothing
        where p = filter (\x -> fst x == key) l

    mapMaybe :: (a -> b) -> Maybe a -> Maybe b
    mapMaybe f Nothing = Nothing
    mapMaybe f (Just x) = Just (f x)

    data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

    

