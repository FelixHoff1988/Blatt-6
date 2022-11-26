
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
data BtreeFloat = Nil | Node Float BtreeFloat BtreeFloat
  deriving (Eq, Show, Ord)

insert :: BtreeFloat -> Float -> BtreeFloat
insert Nil flt = Node flt Nil Nil
insert (Node val left right) flt
  |flt < val = Node val (insert left flt)  right
  |otherwise = Node val left (insert right flt)

depth :: BtreeFloat -> Int
depth Nil = 0
depth (Node val left right)
  |depth left > depth right = 1 + depth left
  |depth left < depth right = 1 + depth right
  |depth left == depth right = 1 + depth right
  |otherwise = 1 + depth right

max_node :: BtreeFloat -> Float
max_node Nil = 0
max_node (Node val Nil Nil) = val
max_node (Node  val  left right) = max_node right

path_diff :: BtreeFloat -> Int
path_diff Nil = depth Nil - shortest_path Nil
path_diff (Node left val right) = depth (Node left val right) - shortest_path (Node left val right)

shortest_path :: BtreeFloat -> Int
shortest_path Nil = 1
shortest_path (Node val left right)
  |shortest_path left > shortest_path right = 1 + shortest_path right
  |shortest_path left < shortest_path right = 1 + shortest_path left
  |shortest_path left == shortest_path right = 1 + shortest_path left
  |otherwise = 1 + shortest_path left

tree :: BtreeFloat
tree = Node 1 (Node 2 (Node 3 (Node 4 Nil Nil) (Node 5 (Node 6 Nil Nil) Nil)) Nil) (Node 7 Nil Nil)