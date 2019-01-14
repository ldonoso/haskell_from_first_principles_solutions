module LibMaybe where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust otherwise = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing otherwise = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just a) = f a


fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]


catMaybes :: [Maybe a] -> [a]
catMaybes = fmap (\ (Just x) -> x) . filter isJust


flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe l
    | all isJust l = Just (catMaybes l)
    | otherwise = Nothing
