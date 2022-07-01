

{-
najlepiej wygląda na drzewach złożonych z jednoelementowych węzłów
użycie - "printTree tree" albo "printTreeWithSlashes tree"
-}


-- ---------------- --
-- testing examples --
-- ---------------- --

smallTree1 :: Tree Int
smallTree1 = Node 3 ( Node 2 Empty Empty ) ( Node 7 Empty Empty )

smallTree2 :: Tree Int
smallTree2 = Node 5 Empty ( Node 4 Empty Empty )

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

charredTree :: Tree String
charredTree =
    Node "a"
    (
        Node "b"
        Empty
        (
            Node "d"
            ( Node "f" Empty Empty )
            Empty
        )
    )
    (
        Node "c"
        (
            Node "e"
            Empty
            ( Node "g" Empty Empty )
        )
        Empty
    )


-- ------------- --
-- implementacja --
-- ------------- --


data Tree a = Empty | Node a ( Tree a ) ( Tree a ) deriving( Show )


showTree :: Show a => Tree a -> Tree String
showTree Empty = Empty
showTree ( Node nodeEl leftTree rightTree ) = Node (show nodeEl) (showTree leftTree) (showTree rightTree)


printTreeWithSlashes :: Show a => Tree a -> IO()
printTreeWithSlashes tree
    = putStrLn ( "\n" ++ ((concat.reverse) (drop 3 (stringifyAndSlashTierTree (tierTree tree) 0 []))) ++ "\n" )

printTree :: Show a => Tree a -> IO()
printTree tree = putStrLn ( "\n" ++ ((concat.reverse) (drop 1 (stringifyTierTree (tierTree tree) 0 []))) ++ "\n" )


printSpaces :: Int -> String
printSpaces times = spaces times ""
    where
        spaces :: Int -> String -> String
        spaces 0 space = space
        spaces times space = spaces (times-1) (space++" ")

inbetween :: Int -> Int
inbetween 0 = 3
inbetween number = (2*( inbetween (number-1))) + 1

before :: Int -> Int
before 0 = 0
before number = (2*( before (number-1) )) + 2



tierTree :: Show a => Tree a -> [[String]]
tierTree tree = nextTier [] [(showTree tree)]

    where

        nextTier :: [[String]] -> [ Tree String ] -> [[String]]
        nextTier soFar listOfTrees = if ( isEmpty listOfTrees )
            then soFar
            else nextTier (soFar++[(listRootNodes listOfTrees)]) (listSubTrees listOfTrees)

        isEmpty :: [ Tree String ] -> Bool
        isEmpty [] = True
        isEmpty (firstTree:otherTrees) = case firstTree of
            Empty -> isEmpty otherTrees
            _ -> False

        listRootNodes :: [ Tree String ] -> [ String ]
        listRootNodes listOfTrees = map getRootNode listOfTrees
            where
                getRootNode :: Tree String -> String
                getRootNode Empty = " "
                getRootNode ( Node rootNode leftTree rightTree ) = rootNode

        listSubTrees :: [ Tree String ] -> [ Tree String ]
        listSubTrees listOfTrees
            = concat [ [leftTree] ++ [rightTree] | ( Node rootNode leftTree rightTree ) <- listOfTrees ]


stringifyTier :: [String] -> Int -> String
stringifyTier tierList tierNo
    = ( ((printSpaces.before) tierNo ) ++ ( tierElements tierList "" tierNo ) )

    where

        tierElements :: [String] -> String -> Int -> String
        tierElements [] elementsString tierNo = elementsString ++ "\n"
        tierElements tierList elementsString tierNo
            = tierElements
                ( tail tierList )
                ( elementsString ++ (head tierList) ++ ((printSpaces.inbetween) tierNo) )
                tierNo


stringifyTierTree :: [[String]] -> Int -> [String] -> [String]
stringifyTierTree tierTree tierNo stringifiedTierTree = stringifyTierTree' (length tierTree) tierTree tierNo stringifiedTierTree
stringifyTierTree' treeDepth [] tierNo stringifiedTierTree = stringifiedTierTree
stringifyTierTree' treeDepth tierTree tierNo stringifiedTierTree
    = stringifyTierTree'
        treeDepth
        ( init tierTree )
        ( tierNo+1 )
        (
            stringifiedTierTree ++
            ["\n"] ++
            [( stringifyTier (last tierTree) tierNo )]
        )


stringifyAndSlashTierTree :: [[String]] -> Int -> [String] -> [String]
stringifyAndSlashTierTree tierTree tierNo stringifiedTierTree = stringifyAndSlashTierTree' (length tierTree) tierTree tierNo stringifiedTierTree
stringifyAndSlashTierTree' treeDepth [] tierNo stringifiedTierTree = stringifiedTierTree
stringifyAndSlashTierTree' treeDepth tierTree tierNo stringifiedTierTree
    = stringifyAndSlashTierTree'
        treeDepth
        ( init tierTree )
        ( tierNo+1 )
        (
            stringifiedTierTree ++
            ["\n\n"] ++
            [( slashes treeDepth tierNo )] ++
            ["\n"] ++
            [( stringifyTier (last tierTree) tierNo )]
        )

    where

        slashes treeDepth 0 = ""
        slashes treeDepth tierNo
            = addSlash
                tierNo
                ( pairNo (treeDepth-tierNo) 1 )
                ( printSpaces (before (tierNo-1)) )


        addSlash :: Int -> Int -> String -> String
        addSlash tierNo 0 slashString = slashString
        addSlash tierNo slashNo slashString =
            addSlash
                tierNo
                ( slashNo-1 )
                (
                    slashString ++
                    " /" ++ ( printSpaces ((inbetween (tierNo-1))-2) ) ++ "\\ " ++
                    ((printSpaces.inbetween) (tierNo-1))
                )

        pairNo :: Int -> Int -> Int
        pairNo 1 counter = counter
        pairNo left counter = pairNo (left-1) (2*counter)


{-
before 0 = 0
before tierNo = tierNo + (before (tierNo-1))
    where
        before :: Int -> Int
        before 0 = 0
        before number = (2*( before (number-1) )) + 2

inbetweenSlash :: Int -> Int
inbetweenSlash 1 = 1
inbetweenSlash 2 = 3
inbetweenSlash number = (2*( inbetweenSlash (number-1) )) - 1

inbetweenPairs 1 = 5
inbetweenPairs number = (number-1)*10 + 1
-}
