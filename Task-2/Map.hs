{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)

data Tree a = Node a (Tree a) (Tree a) | Empty
  deriving (Eq, Ord)

getV :: Tree a -> a
getV (Node v _ _) = v

insertLeafIntoTree :: (Ord a) => Tree a -> Tree a -> Tree a
insertLeafIntoTree Empty leaf = leaf
insertLeafIntoTree old@(Node iv leftTree rightTree) l@(Node v _ _)
  | v < iv = Node iv (insertLeafIntoTree leftTree l) rightTree
  | v > iv = Node iv leftTree (insertLeafIntoTree rightTree l)
  | otherwise = old

insertIntoTree :: (Ord a) => Tree a -> a -> Tree a
insertIntoTree t v = insertLeafIntoTree t (Node v Empty Empty)

findValueTree :: (Ord a) => Tree a -> a -> Maybe a
findValueTree Empty _ = Nothing
findValueTree (Node iv leftTree rightTree) v
  | v < iv = findValueTree leftTree v
  | v > iv = findValueTree rightTree v
  | otherwise = Just iv

treeLeftMost :: (Ord a) => Tree a -> a
treeLeftMost s@(Node v l _) = if l == Empty then v else treeLeftMost l

treeSucc :: (Ord a) => Tree a -> a
treeSucc (Node _ _ rightTree) = treeLeftMost rightTree

deleteValueTree :: (Ord a) => Tree a -> a -> Tree a
deleteValueTree Empty _ = Empty
deleteValueTree s@(Node iv leftTree rightTree) v
  | v < iv = Node iv (deleteValueTree leftTree v) rightTree
  | v > iv = Node iv leftTree (deleteValueTree rightTree v)
deleteValueTree (Node _ Empty Empty) _ = Empty
deleteValueTree (Node _ l Empty) _ = l
deleteValueTree (Node _ Empty r) _ = r
deleteValueTree t@(Node _ leftTree rightTree) _ = Node iv' leftTree rightTree'
  where
    iv' = treeSucc t
    rightTree' = deleteValueTree rightTree iv'

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldl insertIntoTree Empty

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Node v lt rt) = treeToList lt ++ [v] ++ treeToList rt

treeMap :: Tree t -> (t -> a) -> Tree a
treeMap Empty mapper = Empty
treeMap (Node iv leftTree rightTree) mapper = Node (mapper iv) (treeMap leftTree mapper) (treeMap rightTree mapper)

data MapItem k v = Valueable k v | Keyable k

instance (Show k, Show v) => Show (MapItem k v) where
  show (Valueable key value) = show key ++ " -> " ++ show value
  show (Keyable key) = show key

getKey :: MapItem k v -> k
getKey (Valueable key _) = key
getKey (Keyable key) = key

keyOp :: (k -> k -> t) -> MapItem k v -> MapItem k v -> t
keyOp op lhs rhs = op (getKey lhs) (getKey rhs)

instance Eq k => Eq (MapItem k v) where
  (==) = keyOp (==)

instance Ord k => Ord (MapItem k v) where
  compare = keyOp compare

newtype Map k v = Map (Tree (MapItem k v))

findByKey :: Ord k => Map k v -> k -> Maybe (MapItem k v)
findByKey (Map t) key = findValueTree t (Keyable key)

fromList :: Ord k => [MapItem k v] -> Map k v
fromList l = Map (treeFromList l)

(?!) :: Ord k => Map k v -> k -> Maybe (MapItem k v)
map ?! key = findByKey map key

(!) :: Ord k => Map k v -> k -> MapItem k v
map ! key =
  let v = map ?! key
   in fromMaybe (error "Unknown key") v

insert :: Ord k => Map k v -> k -> v -> Map k v
insert (Map t) key value = Map (insertIntoTree t $ Valueable key value)

delete :: Ord k => Map k v -> k -> Map k v
delete (Map t) key = Map (deleteValueTree t $ Keyable key)

toList :: Map k v -> [MapItem k v]
toList (Map t) = treeToList t

mapX :: Map k v -> (v -> w) -> Map k w
mapX (Map t) f = Map (treeMap t ff)
  where ff (Valueable key value) = Valueable key $ f value