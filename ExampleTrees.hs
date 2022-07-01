
{- lab 5 -}
module ExampleTrees
(
    Tree, isTreeEmpty, getNode, getLeftTree, getRigthTree,
    sampleTree, numberTree, numberSubTree, numberOtherTree,
        smallTree1, smallTree2,
    charredTree, postorderThis
)
where



-- ---------------- --
-- data definitions --
-- ---------------- --

data Tree a = Empty | Node a ( Tree a ) ( Tree a )
    deriving( Show )

isTreeEmpty :: Tree a -> Bool
isTreeEmpty Empty = True
isTreeEmpty tree = False

getNode :: Tree a -> a
getNode (Node nodeEl rigthTree leftTree) = nodeEl

getRigthTree :: Tree a -> Tree a
getRigthTree (Node nodeEl rigthTree leftTree) = rigthTree

getLeftTree :: Tree a -> Tree a
getLeftTree (Node nodeEl rigthTree leftTree) = leftTree



-- --------- --
-- int trees --
-- --------- --

sampleTree :: Tree Int
sampleTree =
    Node 3
    ( Node 4 Empty Empty )
    (
        Node 5
        ( Node 4 Empty Empty )
        ( Node 4 Empty Empty )
    )

numberTree :: Tree Int
numberTree =
    Node 1
    (
        Node 2
            ( Node 4 Empty Empty )
            ( Node 5 Empty ( Node 8 Empty Empty ) )
    )
    (
        Node 3
            ( Node 6 Empty ( Node 9 Empty Empty ) )
            ( Node 7 Empty Empty )
    )

numberSubTree :: Tree Int
numberSubTree =
    Node 2
        ( Node 4 Empty Empty )
        ( Node 5 Empty ( Node 8 Empty Empty ) )

numberOtherTree :: Tree Int
numberOtherTree =
    Node 3
    ( Node 3 Empty Empty )
    (
        Node 4
        ( Node 2 Empty Empty )
        Empty
    )

smallTree1 :: Tree Int
smallTree1 = Node 3 ( Node 2 Empty Empty ) ( Node 7 Empty Empty )

smallTree2 :: Tree Int
smallTree2 = Node 5 Empty ( Node 4 Empty Empty )



-- ---------- --
-- char trees --
-- ---------- --

charredTree :: Tree Char
charredTree =
    Node 'a'
    (
        Node 'b'
        Empty
        (
            Node 'd'
            ( Node 'f' Empty Empty )
            Empty
        )
    )
    (
        Node 'c'
        (
            Node 'e'
            Empty
            ( Node 'g' Empty Empty )
        )
        Empty
    )

postorderThis :: Tree Char
postorderThis =
    (
        Node 'e'
        (
            Node 't'
            (
                Node 'a'
                ( Node 'g' Empty Empty )
                ( Node 'r' Empty Empty )
            )
            Empty
        )
        (
            Node 'j'
            ( Node 'u' Empty Empty )
            (
                Node 'u'
                Empty
                ( Node 'l' Empty Empty )
            )
        )
    )
