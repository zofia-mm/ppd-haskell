
import ExampleTrees {- do zadania trzeciego -}



-- ---------------- --
-- zadanie pierwsze --
-- ---------------- --

import Set {- w osobnym pliku -}

-- dzięki temu możemy korzystać z funkcji zawartych w module Set,
-- takich jak setify czy union



-- --------------- --
-- zadanie trzecie --
-- --------------- --

{- import ExampleTrees -}

-- dzięki czemu mamy dostęp do klasy Tree oraz możemy korzystać
-- z przykładowych drzew

shortestBranchLength :: Tree a -> Int
shortestBranchLength tree = innerSBL tree 0
    where
        innerSBL tree branchLength = if isTreeEmpty tree
            then branchLength-1
            else min
                    (innerSBL (getLeftTree tree)  branchLength+1)
                    (innerSBL (getRigthTree tree) branchLength+1)

longestBranchLength :: Tree a -> Int
longestBranchLength tree = innerLBL tree 0
    where
        innerLBL :: Tree a -> Int -> Int
        innerLBL tree branchLength = if isTreeEmpty tree
            then branchLength-1
            else max
                    (innerLBL (getLeftTree tree)  branchLength+1)
                    (innerLBL (getRigthTree tree) branchLength+1)



-- --------------- --
-- zadanie czwarte --
-- --------------- --

data Teree a = Empty | Node a (Teree a) (Teree a) (Teree a)
    deriving ( Show )

sumTeree :: Teree Int -> Int
sumTeree Empty = 0
sumTeree teree = innerSum teree 0
    where
        innerSum :: Teree Int -> Int -> Int
        innerSum Empty summer = summer
        innerSum (Node nodeEl teree1 teree2 teree3) summer =
            summer + nodeEl +
            (innerSum teree1 0) + (innerSum teree2 0) + (innerSum teree3 0)

newTeree :: Teree Int
newTeree =
    Node 11
    ( Node 21 Empty Empty Empty )
    (
        Node 31
        Empty
        ( Node 32 Empty Empty Empty )
        Empty
    )
    (
        Node 22
        ( Node 33 Empty Empty Empty )
        Empty
        ( Node 34 Empty Empty Empty )
    )

smallTeree :: Teree Int
smallTeree =
    Node 1
    ( Node 2 Empty Empty Empty )
    (
        Node 2
        Empty
        ( Node 3 Empty Empty Empty )
        Empty
    )
    (
        Node 2
        ( Node 3 Empty Empty Empty )
        Empty
        ( Node 3 Empty Empty Empty )
    )
