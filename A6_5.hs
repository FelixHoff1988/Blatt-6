
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
--Datentyp Btreefloat der nur Elemente vom Typ Float abspeichert
data BtreeFloat = Nil | Node Float BtreeFloat BtreeFloat
  deriving (Eq, Show, Ord)

--Funktion die eine Gleitkommazahl in den Binärbaum einfügt. Fur jeden Knoten wird der Wert s mit dem Wert des aktuellen Knoten si verglichen. Ist s < si dann wird der Wert in den linken Binärbaum eingefügt, ist s > si dann wird der Wert in den rechten Binärbaum eingefügt. Sind beide Werte gleich wird der Wert nicht eingefügt. Der Wert s wird soweit nach unten durchgereicht bis ein Blatt erreicht wird. Dort wird dann ein neuer Knoten mit dem Wert s und zwei leeren Kindern (Binärbäumen) eingefugt
insert :: BtreeFloat -> Float -> BtreeFloat
insert Nil flt = Node flt Nil Nil
insert (Node val left right) flt
  |flt < val = Node val (insert left flt)  right
  |otherwise = Node val left (insert right flt)

--Funktion die die Tiefe eines Binärbaumes angibt. Dabei ist die Tiefe beschrieben durch den maximal längsten Pfad von der Wurzel bis zu einem Blatt
depth :: BtreeFloat -> Int
depth Nil = 0
depth (Node val left right)
  |depth left > depth right = 1 + depth left
  |depth left < depth right = 1 + depth right
  |depth left == depth right = 1 + depth right
  |otherwise = 1 + depth right

--Funktion die den größten Knotenwert innerhalb des Binärbaumes zurückgibt. Sollte die Eingabe nur ein leerer Knoten sein, dann soll max_node den Wert 0 zuruckgeben
max_node :: BtreeFloat -> Float
max_node Nil = 1
max_node (Node val Nil Nil) = val
max_node (Node  val  left right) = max_node right

--Funktion die die Differenz zwischen dem kürzesten und dem längsten Pfad im Binärbaum zurückgibt. Ein Pfad sei dabei die Anzahl der Knoten vom Wurzelknoten (dem Startknoten) zu einem Blattknoten
path_diff :: BtreeFloat -> Int
path_diff Nil = depth Nil - shortest_path Nil
path_diff (Node left val right) = depth (Node left val right) - shortest_path (Node left val right)

--Hilfsfunktion die die minimale Tiefe eines Binärbaumes angibt. Dabei ist die minimale Tiefe beschrieben durch den maximal kürzesten Pfad von der Wurzel bis zu einem Blatt
shortest_path :: BtreeFloat -> Int
shortest_path Nil = 1
shortest_path (Node val left right)
  |shortest_path left > shortest_path right = 1 + shortest_path right
  |shortest_path left < shortest_path right = 1 + shortest_path left
  |shortest_path left == shortest_path right = 1 + shortest_path left
  |otherwise = 1 + shortest_path left

--tree aus dem Beispiel
tree :: BtreeFloat
tree = Node 1 (Node 2 (Node 3 (Node 4 Nil Nil) (Node 5 (Node 6 Nil Nil) Nil)) Nil) (Node 7 Nil Nil)