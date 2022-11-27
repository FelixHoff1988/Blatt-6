{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
-- Die Datentyp BtreeFloat besteht aus einem leerem Knoten Nil oder ein Knoten Node mit einer Gleitkommazahl (Float) als Wert und einem linken Kind (einem Bin¨arbaum) und einem rechten Kind (einem Bin¨arbaum).
data BtreeFloat = Nil | Node Float BtreeFloat BtreeFloat
  deriving (Eq, Show, Ord)
-- Die Funktion fügt eine Gleitkommazahl in den Binärbaum ein. Diese Funktion fugt eine Gleitkommazahl in den Bin ¨ ¨arbaum ein. Fur jeden Knoten wird der Wert s mit dem Wert des aktuellen Knoten s^i verglichen. Ist s < s^i dann wird der Wert in den linken Bin¨arbaum eingefugt, ist s > s^i dann wird der Wert in den rechten Binärbaum eingefugt. Sind beide Werte gleich wird der Wert nicht eingefügt. Der Wert s wird soweit nach unten durchgereicht bis ein Blatt erreicht wird. Dort wird dann ein neuer Knoten mit dem Wert s und zwei leeren Kindern (Binäarbäumen) eingefügt
--Beispiel insert tree 2 sollte Node 1.0 (Node 2.0 (Node 3.0 (Node 4.0 Nil Nil) (Node 5.0 (Node 6.0 Nil Nil) Nil)) Nil) (Node 7.0 (Node 2.0 Nil Nil) Nil) ausgeben.
insert :: BtreeFloat -> Float -> BtreeFloat
insert Nil flt = Node flt Nil Nil
insert (Node val left right) flt
  |flt < val = Node val (insert left flt)  right
  |otherwise = Node val left (insert right flt)
-- Die Funktion gibt die Tiefe des Binärbaumes an. Die Tiefe ist dabei der maximal Längste Pfad von der Wurzel bis zu einem Blatt.
-- Beispiel depth tree sollte 5 ausgeben
depth :: BtreeFloat -> Int
depth Nil = 0
depth (Node val left right)
  |depth left > depth right = 1 + depth left
  |depth left < depth right = 1 + depth right
  |depth left == depth right = 1 + depth right
  |otherwise = 1 + depth right
-- Die Funktion gibt den größten Knotenwert innerhalb eines Binärbaumes zurück, im falle eines leeren Knoten ist die Ausgabe 0.
-- Beispiel max_node tree ist 7.0
max_node :: BtreeFloat -> Float
max_node Nil = 0
max_node (Node val Nil Nil) = val
max_node (Node  val  left right) = max_node right
-- Die Funktion gibt die Differenz zwischen dem kürzesten und dem längsten Pfad im Binärbaum zurück. Ein Pfad sei dabei die Anzahl der Knoten vom WUrzelknoten (dem Startknoten) zu einem Blattknoten.
-- Beispiel path_diff tree sollte 2 ausgeben
path_diff :: BtreeFloat -> Int
path_diff Nil = depth Nil - shortest_path Nil
path_diff (Node left val right) = depth (Node left val right) - shortest_path (Node left val right)
-- Hilfsfunktion um die Funktion path_diff implementieren zu können
shortest_path :: BtreeFloat -> Int
shortest_path Nil = 1
shortest_path (Node val left right)
  |shortest_path left > shortest_path right = 1 + shortest_path right
  |shortest_path left < shortest_path right = 1 + shortest_path left
  |shortest_path left == shortest_path right = 1 + shortest_path left
  |otherwise = 1 + shortest_path left
-- Konstruktor für einen Binärbaum BtreeFloat
tree :: BtreeFloat
tree = Node 1 (Node 2 (Node 3 (Node 4 Nil Nil) (Node 5 (Node 6 Nil Nil) Nil)) Nil) (Node 7 Nil Nil)
