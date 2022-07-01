
-- -- zadanie pierwsze

insertAtFirst :: Integer -> [ Integer ] -> [ Integer ]
insertAtFirst valueToInsert list = valueToInsert : list

insertAtSecond :: Integer -> [ Integer ] -> [ Integer ]
insertAtSecond valueToInsert list =
    [ head list ] ++ [ valueToInsert ] ++ tail list

insertAtSecond2 :: Integer -> [ Integer ] -> [ Integer ]
insertAtSecond2 valueToInsert ( lHead:lTail ) =
    lHead : valueToInsert : lTail

insertLast :: Integer -> [ Integer ] -> [ Integer ]
insertLast valueToInsert list = list ++ [ valueToInsert ]


-- -- zadanie drugie

returnSecond list = list !! 1

returnSecond2 ( lHead : secondElement : lTail ) = secondElement

returnThird list = list !! 2

returnAlmostLast list = head( tail( reverse list ) )


-- -- zadanie trzecie

switchFirstAndLast ( lHead : lTail ) = last lTail : init lTail ++ [ lHead ]


-- -- zadanie czwarte a

leaveEven :: [Integer] -> [Integer] -> [Integer]
leaveEven [] list = list
leaveEven ( lHead : lTail ) list =
    if mod lHead 2 == 0
        then leaveEven lTail list ++ [ lHead ]
        else leaveEven lTail list
    where list = []

countEven :: [Integer] -> Integer
countEven list =
    countEvenInternal list 0
    where
        countEvenInternal :: [Integer] -> Integer -> Integer
        countEvenInternal [] counter = counter
        countEvenInternal ( lHead : lTail ) counter =
            if ( mod lHead 2 ) == 0
                then countEvenInternal lTail counter + 1
                else countEvenInternal lTail counter


-- -- zadanie czwarte b

-- wersja z listą
countDiv3Untill :: Integer -> Integer
countDiv3Untill finishAt = countDiv3List [ 1..finishAt ] 0
    where
        countDiv3List :: [Integer] -> Integer -> Integer
        countDiv3List [] counter = counter
        countDiv3List (lHead:lTail) counter = if (mod lHead 3) == 0
            then countDiv3List lTail counter+1
            else countDiv3List lTail counter

-- wersja ze wzorem
simpleCountDiv3Untill :: Integer -> Integer
simpleCountDiv3Untill finishAt = div finishAt 3


-- -- zadanie czwarte c

-- wersja z listą
addDiv3Untill :: Integer -> Integer
addDiv3Untill finishAt = addDiv3List [ 1..finishAt ] 0
    where
        addDiv3List :: [Integer] -> Integer -> Integer
        addDiv3List [] counter = counter
        addDiv3List (lHead:lTail) counter = if (mod lHead 3) == 0
            then addDiv3List lTail counter+lHead
            else addDiv3List lTail counter

-- wersja ze wzorem
simpleAddDiv3Untill :: Integer -> Integer
simpleAddDiv3Untill finishAt = 3 * sumNum 0 (div finishAt 3) 0
    where
        sumNum :: Integer -> Integer -> Integer -> Integer
        sumNum currentlyAt finishAt summer =
            if currentlyAt > finishAt
                then summer
                else sumNum (currentlyAt+1) finishAt (summer+currentlyAt)


-- -- zadanie piąte

isListLengthOddOrEven list = if list == []
    then "lista pusta"
    else isListLengthOdd list
    where
        isListLengthOdd list = if (tail list) == []
            then "lista o nieparzystej liczbie elementow"
            else isListLengthEven (tail list)
        isListLengthEven list = if (tail list) == []
            then "lista o parzystej liczbie elementow"
            else isListLengthOdd (tail list)

simpleIsListLengthOddOrEven list = ( mod (length list) 2 ) == 0


-- -- zadanie szóste

sqrListMap :: (Num x) => [x] -> [x]
sqrListMap list = map sqr list
    where
        sqr :: (Num x) => x -> x
        sqr x = x * x

sqrListNoMap :: (Num x) => [x] -> [x]
sqrListNoMap list = sqrMove list []
    where
        sqrMove :: (Num x) => [x] -> [x] -> [x]
        sqrMove [] listTo = listTo
        sqrMove listFrom listTo =
            sqrMove ( tail listFrom ) ( listTo ++ [ ( head listFrom )^2 ] )


-- -- zadanie siódme

countElementInList element list = countElement element list 0
    where
        countElement element [] counter = counter
        countElement element list counter = if element == ( head list )
            then countElement element (tail list) counter+1
            else countElement element (tail list) counter
