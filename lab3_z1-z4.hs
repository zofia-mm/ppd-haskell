
-- -- zadanie pierwsze a, rekurencyjnie

printSequenceRec :: Integer -> Integer -> Integer -> [Integer]
printSequenceRec firstEl lastEl step = makeSequenceRec firstEl lastEl step []
    where
        makeSequenceRec :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
        makeSequenceRec currentEl lastEl step list = if currentEl > lastEl
            then list
            else makeSequenceRec (currentEl+step) lastEl step (list++[currentEl])


-- -- zadanie pierwsze b, bez rekurencji

printSequence :: Integer -> Integer -> Integer -> [Integer]
printSequence firstEl lastEl step
    = [ x | x <- [ firstEl, (firstEl+step) .. lastEl ] ]


-- -- zadanie drugie a

fromListNthtoMth :: [a] -> Int -> Int -> [a]
fromListNthtoMth list n m = goThroughAndCollect list (n-1) (m-1) 0 []
    where
        goThroughAndCollect list n m i listOut = if i > m
            then listOut
            else if i >= n
                then goThroughAndCollect list n m (i+1) (listOut++[list!!i])
                else goThroughAndCollect list n m (i+1) listOut


-- -- zadanie drugie b

sliceFromList :: [a] -> Int -> Int -> [a]
sliceFromList list startAt stopAt = drop ( startAt-1 ) ( take stopAt list )


-- -- zadanie trzecie

enumerate :: Integer -> [a] -> [ (Integer, a) ]
enumerate startAt list = zip [startAt..] list


-- -- zadanie czwarte

{-

    (a)
foldr (/) 2 [6,12,24,8]
= 6/ ( 12/ ( 24/ ( 8/2 ))) = ...

foldr (/) 2 [6,12,24,8]<-
= foldr (/) (8/2)  [6,12,24]
= foldr (/) (24/4) [6,12]
= foldr (/) (12/6) [6]
= (6/2) = 3

    (b)
foldr (&&) True [ 1>2, 3>2, 5==5 ]
= 1>2 && ( 3>2 && ( 5==5 && True )) = ...

foldr (&&) True [ 1>2, 3>2, 5==5 ]<-
= foldr (&&) (5==5 && True) [ 1>2, 3>2 ]
= foldr (&&) (3>2 && True)  [ 1>2 ]
= (1>2 && True) = False

    (c)
foldr max 18 [3,6,12,4,55,11]<-
= foldr max ( max 18 11 ) [3,6,12,4,55]
= foldr max ( max 18 55 ) [3,6,12,4]
= foldr max ( max 55 4 )  [3,6,12]
= foldr max ( max 55 12 ) [3,6]
= foldr max ( max 55 6 )  [3]
= ( max 55 3 ) = 55

    (d)
foldr ( \x y -> (x+y)/2 ) 54 [24,4,10,6]<-
= foldr (...) ( (54+6)/2 )  [24,4,10]
= foldr (...) ( (30+10)/2 ) [24,4]
= foldr (...) ( (20+4)/2 )  [24]
= ( (12+24)/2 ) = 18

    (e)
foldl ( \x y -> (x+y)/2 ) 54 ->[2,4,10,6]
= foldl (...) ( (54+2)/2 )  [4,10,6]
= foldl (...) ( (28+4)/2 )  [10,6]
= foldl (...) ( (16+10)/2 ) [6]
= ( (13+6)/2 ) = 9,5

    (f)
foldl (/) 64 ->[4,2,4]
= foldl (/) (64/4) [2,4]
= foldl (/) (16/2) [4]
= (8/4) = 2

    (g)
foldl ( \x y -> 2*x + y ) 8 ->[1,2,3]
= foldl (...) (2*8 + 1)  [2,3]
= foldl (...) (2*17 + 2) [3]
= (2*36 + 3) = 75

-}
