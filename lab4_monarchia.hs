

{-
tak mi się skojarzyło, że w bryjskiej monarchii dziedziczenie tronu
przypomina preorder ( tylko panie są ostatnie wśród rodzeństwa ),
to proszę, można sobie popatrzeć po wywołaniu tych funcji bezargumentowych
implementacja niżej
-}

printSuccesionLine =
    ( putStrLn.concat.map ( \(x,y) -> (show x) ++ ". " ++ (printPerson y) ++ "\n" ) )
    ( zip [1..] (preorderNonBinaryPerson royalFamily) )

printRoyalFamily = printNonBinaryTree royalFamily



-- -- potrzebuję drzewa nie binarnego

{-
zwykłe drzewo dla porównania
data Tree a = Empty | Node a ( Tree a ) ( Tree a ) deriving( Show )

muszę też użyć innych nazw niż Empty oraz Node,
bo zwykłe drzewo je zadeklarowało
-}

                                      {- zamiast dwóch, dajemy listę -}
data NonBinaryTree a = Empty | Node a [ NonBinaryTree a ] deriving( Show )



-- -- klasa person, żeby ładniej było

type Name = [Char]
type Surname = [Char]
type Title = [Char]
type Person = ( Name, Title )

printPerson :: Person -> String
printPerson ( name, "" ) = name
printPerson ( name, title ) = title ++ " " ++ name



-- -- drzewo samo w sobie

{-
po kolei, od królowej
dzieci w liście: panowie, panie; od najstarszych do najmłodszych
-}

royalFamily :: NonBinaryTree Person
royalFamily =
    (
        Node ( "Elizabeth", "Queen" )
        [
            (
                Node ( "Charles", "Prince Of Wales" )
                [
                    Node( "William", "Prince, Duke of Cambridge" )
                    [
                        ( Node( "George", "Prince" ) [Empty] ),
                        ( Node( "Louis", "Prince" ) [Empty] ),
                        ( Node( "Charlotte", "Princess" ) [Empty] )
                    ],
                    Node( "Harry", "Prince, Duke Of Sussex" )
                    [
                        ( Node( "Archie", "Prince" ) [Empty] )
                    ]
                ]
            ),
            (
                Node ( "Andrew", "Prince, Duke Of York" )
                [
                    ( Node ( "Eugenie", "Princess" ) [Empty] ),
                    ( Node ( "Beatrice", "Princess" ) [Empty] )
                ]
            ),
            (
                Node ( "Edward", "Prince, Earl Of Wessex" )
                [
                    ( Node( "James", "Viscount Severn" ) [Empty] ),
                    ( Node( "Louise", "Lady" ) [Empty] )
                ]
            ),
            (
                Node ( "Anne", "Princess Royal" )
                [
                    (
                        Node ( "Zara", "" )
                        [
                            ( Node( "Lucas Philip", "" ) [Empty] ),
                            ( Node( "Mia", "" ) [Empty] ),
                            ( Node( "Lena", "" ) [Empty] )
                        ]
                    ),
                    ( Node ( "Peter", "" ) [Empty] )
                ]
            )
        ]
    )



-- -- funkcje

printNonBinaryTree :: NonBinaryTree Person -> IO()
printNonBinaryTree ( Node nodeValue treeList )
    = printNonBinaryTree' ( Node nodeValue treeList ) 0

    where

        printNonBinaryTree' :: NonBinaryTree Person -> Int -> IO()
        printNonBinaryTree' Empty step = return ()
        printNonBinaryTree' ( Node nodeValue treeList ) step = do
            putStrLn  ( ( printStep step ) ++ ( printPerson nodeValue ) )
            mapM_ ( \x -> printNonBinaryTree' x (step+1) ) treeList

        printStep :: Int -> String
        printStep stepValue = ( replicate (2*(stepValue-1)) ' ' ) ++ "└--->"


preorderNonBinaryPerson :: NonBinaryTree Person -> [ Person ]
preorderNonBinaryPerson Empty = []
preorderNonBinaryPerson ( Node nodeValue treeList )
    = [ nodeValue ] ++ ( goThroughList treeList )
    where
        goThroughList :: [ NonBinaryTree Person ] -> [ Person ]
        goThroughList [] = []
        goThroughList ( first:other ) =
            ( preorderNonBinaryPerson first ) ++ ( goThroughList other )
