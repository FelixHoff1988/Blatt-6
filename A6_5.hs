module BtreeFloat (BtreeFloat(Nil,Node), 
    isEmpty, value, lft, rght, isLeaf) 
    where

data BtreeFloat a = Nil | Node a (BtreeFloat a) (BtreeFloat a)
    deriving (Show)
    --Baum ist leer oder hat einen Knoten mit Wert
    --wobei der Knoten zwei Kinder hat, die auch leer sein können

isEmpty :: BtreeFloat a -> Bool                --testet, ob Baum leer ist
isEmpty Nil = True                             --leerer Baum
isEmpty (Node nValue tLeft tRight) = False     --nicht leerer Baum

value :: BtreeFloat a -> a                     --Ausleseoperation
value Nil = error "Baum ist leer"              --Zugriff nicht erlaubt
value(Node nValue tLeft tRight) = nValue       --Liefert Wert des Knotens

lft :: BtreeFloat a -> BtreeFloat a            --Linker Teilbaum
lft Nil = error "Baum ist leer"                --Zugriff nicht erlaubt
lft(Node nValue tLeft tRight) = tLeft          --linkes Kind

rght :: BtreeFloat a -> BtreeFloat a           --Rechter Teilbaum
rght Nil = error "Baum ist leer"               --Zugriff nicht erlaubt
rght (Node nValue tLeft tRight) = tRight       --rechtes Kind

isLeaf :: BtreeFloat a -> Bool                 --testet, ob Baum nur ein Blatt ist
isLeaf Nil = error "Baum ist leer"             --Zugriff nicht erlaubt
isLeaf (Node nValue Nil Nil) = True            --Blatt hat keine Kinder
isLeaf ( Node nValue tLeft tRight) = False     --sonst kein Blatt

tree :: BtreeFloat Float
tree = Node 1 (Node 2 (Node 3 (Node 4 Nil Nil) (Node 5 (Node 6 Nil Nil) Nil)) Nil) (Node 7 Nil Nil)

insert :: BtreeFloat a-> Float -> BtreeFloat a
insert node s
    | s < value node = tRight
