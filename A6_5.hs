module BtreeFloat (BtreeFloat(Nil,Node), isEmpty, value, lft, rght, isLeaf) where
import Control.Arrow (ArrowChoice(left, right))
import Distribution.Compat.Graph (nodeValue)
import Graphics.Win32 (tA_LEFT)

data BtreeFloat a = Nil | Node a (BtreeFloat a) (BtreeFloat a)
    deriving (Show)

isEmpty     :: BtreeFloat a -> Bool
value       :: BtreeFloat a -> a
lft        :: BtreeFloat a -> BtreeFloat a
rght       :: BtreeFloat a -> BtreeFloat a
isLeaf      :: BtreeFloat a -> Bool

isEmpty Nil = True                              --leerer Baum
isEmpty (Node nValue tLeft tRight) = False      --nicht leerer Baum

value Nil = error "Baum ist leer"               --Zugriff nicht erlaubt
value(Node nValue tLeft tRight) = nValue     --Liefert Wert des Knotens

lft Nil = error "Baum ist leer"                --Zugriff nicht erlaubt
lft(Node nValue tLeft tRight) = tLeft          --linkes Kind

rght Nil = error "Baum ist leer"               --Zugriff nicht erlaubt
rght (Node nValue tLeft tRight) = tRight       --rechtes Kind

isLeaf Nil = error "Baum ist leer"              --Zugriff nicht erlaubt
isLeaf (Node nValue Nil Nil) = True             --Blatt hat keine Kinder
isLeaf ( Node nValue tLeft tRight) = False      --sonst kein Blatt