

-- -------------------- --
-- implementacja drzewa --
-- -------------------- --

data Tree a = Empty | Node a ( Tree a ) ( Tree a )
    deriving( Show )

node :: Eq a => Tree a -> a
node ( Node nodeEl leftTree rightTree ) = nodeEl

left :: Eq a => Tree a -> Tree a
left ( Node nodeEl leftTree rightTree ) = leftTree

right :: Eq a => Tree a -> Tree a
right ( Node nodeEl leftTree rightTree ) = rightTree



-- ------------------ --
-- przykładowe drzewa --
-- ------------------ --

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



-- -------------- --
-- zadanie szóste --
-- -------------- --

inSubtreeOf :: Eq a => Tree a -> Tree a -> Bool
inSubtreeOf Empty _ = False
inSubtreeOf _ Empty = False
inSubtreeOf maybeSubtree tree = if ( equalTrees maybeSubtree tree )
    then True
    else ( inSubtreeOf maybeSubtree (left tree) ) ||
         ( inSubtreeOf maybeSubtree (right tree) )

equalTrees :: Eq a => Tree a -> Tree a -> Bool
equalTrees Empty Empty = True
equalTrees Empty _ = False
equalTrees _ Empty = False
equalTrees tree1 tree2 = if (node tree1) == (node tree2)
    then ( equalTrees (left tree1) (left tree2) ) &&
         ( equalTrees (right tree1) (right tree2) )
    else False



-- -------------- --
-- zadanie siódme --
-- -------------- --

flattenTreeByTier :: Tree a -> [a]
flattenTreeByTier tree = nextTier [] [tree]

    where

        nextTier :: [a] -> [ Tree a ] -> [a]
        nextTier tierList treeList = if ( fullOfEmptiness treeList )
            then tierList
            else nextTier
                    ( tierList ++ (listRootElements treeList) )
                    ( listSubTrees treeList )

        fullOfEmptiness :: [ Tree a ] -> Bool
        fullOfEmptiness [] = True
        fullOfEmptiness (firstTree:otherTrees) = case firstTree of
            Empty -> fullOfEmptiness otherTrees
            _ -> False

        listRootElements :: [ Tree a ] -> [ a ]
        listRootElements treeList = [ rootEl | ( Node rootEl leftTree rightTree ) <- treeList ]

        listSubTrees :: [ Tree a ] -> [ Tree a ]
        listSubTrees treeList
            = concat [ [leftTree] ++ [rightTree] | ( Node rootEl leftTree rightTree ) <- treeList ]
