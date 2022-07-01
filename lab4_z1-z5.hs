

-- ---------------- --
-- zadanie pierwsze --
-- ---------------- --

{-
wkleić do ghci, podać argumenty
(a)     ( \x -> x*3 )
(b)     ( \x y -> x*y )
(c)     ( \x y z -> x+y+z )
-}



-- -------------- --
-- zadanie drugie --
-- -------------- --


{- typ -}   {- konstruktory -}
data Moto = Ford | BMW | Mazda | Kia | Skoda
    deriving( Show ) {- <- aby dało się wyświetlać -}


-- -- zadanie drugie a

{- synonim typu -}
type Kraj = [Char]

markaZKraju :: Kraj -> Moto
markaZKraju kraj = case kraj of
    "Ameryka" -> Ford
    "Niemcy" -> BMW
    "Japonia" -> Mazda
    "Korea" -> Kia
    "Czechy" -> Skoda


-- -- zadanie drugie b

maxSpeed :: Moto -> Integer
maxSpeed markaSamochodu = case markaSamochodu of
    Ford -> 200
    Skoda -> 300
    BMW -> 290
    Mazda -> 150
    Kia -> 447000


{-
bonus: złożenie funkcji (jakby ktoś chciał potestować)
przekleić do ghci
    maxSpeed ( markaZKraju "Czechy" )
    (maxSpeed.markaZKraju) "Czechy"
(z nawiasem, żeby było wiadomo, gdzie się kończy funkcja, a zaczynają argumenty)
-}



-- --------------- --
-- zadanie trzecie --
-- --------------- --


data Uczelnia = UAM | UJ | AGH | PP | UW
    deriving( Show )

-- bonus: synonimy typów
type Rok = Integer
type Miasto = String

-- bez tamtych synonimów można zrobić po prostu
-- informacje :: Uczelnia -> ( Integer, String )
informacje :: Uczelnia -> ( Rok, Miasto )
informacje uczelnia = case uczelnia of
    UAM -> ( 1919, "Poznan" )
    UJ -> ( 1364, "Krakow" )
    AGH -> ( 1919, "Krakow" )
    PP -> ( 1919, "Poznan" )
    UW -> ( 1702, "Wroclaw" )



-- --------------- --
-- zadanie czwarte --
-- --------------- --


-- jakby ktoś chciał wizualnie się upewnić, czy dobrze przepisał drzewo,
-- to dodam jeszcze w marcu na teamsy plik z funkcją printTree


-- przepisane z wykładu
-- (jak ktoś chce samemu spróbować napisać preorder, inorder, postorder
-- to na samym dole są bonusowe drzewa)

data Tree a = Empty | Node a ( Tree a ) ( Tree a )
    deriving( Show )

                {- a to typ elementów w drzewie -}
inorder :: Tree a -> [a]
inorder Empty = []
inorder ( Node nodeElement leftTree rightTree )
    = ( inorder leftTree ) ++ [ nodeElement ] ++ ( inorder rightTree )

preorder :: Tree a -> [a]
preorder Empty = []
preorder ( Node nodeElement leftTree rightTree )
    = [ nodeElement ] ++ ( preorder leftTree ) ++ ( preorder rightTree )

postorder :: Tree a -> [a]
postorder Empty = []
postorder ( Node nodeElement leftTree rightTree )
    = ( postorder leftTree ) ++ ( postorder rightTree ) ++ [ nodeElement ]


-- -- zadanie czwarte a

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


-- -- zadanie czwarte b

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



-- ------------- --
-- zadanie piąte --
-- ------------- --


-- -- zadanie piąte a

tree_elem :: Eq a => a -> Tree a -> Bool
tree_elem element tree = elem element ( inorder tree )


-- -- zadanie piąte b

-- order w nazwie, bo przechodzi po drzewie, podobnie do funkcji ...order
tree_member_order :: Eq a => a -> Tree a -> Bool
-- jeśli drzewo jest puste / dojdziemy do pustego krańca
tree_member_order element Empty = False
tree_member_order element ( Node nodeEl leftEl rightEl ) =
    if( nodeEl == element )
        -- żeby nie wywoływać do pustego dołu drzewa, jeśli już mamy w którymś węźle
        -- to True idzie na 'górę' po rekurencji
        then True
        -- bierzemy alternatywę, aby sprawdzić obie gałęzie
        -- dostajemy od gałęzi albo False (kiedy dojdzie do pustego krańca)
        -- albo True (jeśli któryś węzeł po drodze do dołu jest równy elementowi)
        else ( tree_member_order element leftEl ) || ( tree_member_order element rightEl )



-- ---------------- --
-- bonusowe drzewa --
-- ---------------- --


-- -- tree schema

exampleTree :: Tree Int
exampleTree =
    (

        {- ten wezeł -}
        Node 8

        {- <- lewe poddrzewo -}
        ( Node 7 Empty Empty )

        {- prawe poddrzewo -> -}
        (
            {- prawy węzeł -}
            Node 3
            {- <- lewe poddrzewo prawego węzła -}
            ( Node 3 Empty Empty )
            {- prawe poddrzewo prawego węzła -> -}
            ( Node 4 Empty Empty )
        )

    )


-- -- simple ones

inorderTree :: Tree Int
inorderTree =
    Node 5
    (
        Node 4
        (
            Node 2
            ( Node 1 Empty Empty )
            ( Node 3 Empty Empty )
        )
        Empty
    )
    (
        Node 8
        (
            Node 6
            Empty
            ( Node 7 Empty Empty )
        )
        ( Node 9 Empty Empty )
    )

postorderTree :: Tree Int
postorderTree =
    Node 9
    (
        Node 4
        Empty
        (
            Node 3
            ( Node 1 Empty Empty )
            ( Node 2 Empty Empty )
        )
    )
    (
        Node 8
        Empty
        (
            Node 7
            ( Node 5 Empty Empty )
            ( Node 6 Empty Empty )
        )
    )

preorderTree :: Tree Int
preorderTree =
    Node 1
    (
        Node 2
        ( Node 3 Empty Empty )
        (
            Node 4
            ( Node 5 Empty Empty )
            ( Node 6 Empty Empty )
        )
    )
    (
        Node 7
        ( Node 8 Empty Empty )
        ( Node 9 Empty Empty )
    )


-- -- secret postorder messages

postorderPassword :: Tree Char
postorderPassword =
    (
        Node 'd'
        (
            Node 's'
            (
                Node 'a'
                Empty
                ( Node 'p' Empty Empty )
            )
            Empty
        )
        (
            Node 'r'
            ( Node 's' Empty Empty )
            (
                Node 'o'
                ( Node 'w' Empty Empty )
                Empty
            )
        )
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
