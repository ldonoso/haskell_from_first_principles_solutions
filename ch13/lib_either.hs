module LibEither where

lefts' :: [Either a b] -> [a]
lefts' = foldr (\x l -> getLeft x ++ l) []
    where getLeft x = case x of
                        Left x' -> [x']
                        otherwise -> []

rights' :: [Either a b] -> [b]
rights' = foldr (\x l -> getRigth x ++ l) []
    where getRigth x = case x of
                        Right x' -> [x']
                        otherwise -> []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' l = (lefts' l, rights' l)


eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa _ (Left a) = fa a
either' _ fb (Right b) = fb b 

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\ _ -> Nothing) (Just . f)
