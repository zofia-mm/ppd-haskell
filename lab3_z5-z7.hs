
-- -- zadanie piąte

nalezy :: Eq a => a -> [a] -> Bool
nalezy element lista = foldl
    (\ soFar nextListElem -> (soFar || (element == nextListElem)) )
    False lista

{-
element = 3
nalezy element lista
= foldl (...) ( False )               [1,2,3,4]
= foldl (...) ( False||(1==element) ) [2,3,4]
= foldl (...) ( False||(2==element) ) [3,4]
= foldl (...) ( False||(3==element) ) [4]
= foldl (...) ( True ||(4==element) ) []
= True
-}


-- -- zadanie szóste

leftFoldedMap :: ( a -> b ) -> [a] -> [b]
leftFoldedMap fun list = foldl
    ( \ done nextEl -> ( done ++ [ ( fun nextEl) ] ) )
    [] list

{-
fun x = 3*x
leftFoldedMap fun list
= foldl (...) []               [1,2,3]
= foldl (...) [] ++[(fun 1)]   [2,3]
= foldl (...) [3]++[(fun 2)]   [3]
= foldl (...) [3,6]++[(fun 3)] []
= [3,6,9]
-}


-- -- ogarnięcie foldr1, foldl1

{-

    (a)
foldr1 (+) [1,2,3,4]<-
= 1+ ( foldr1 (+) [2,3,4] )
= 1+ ( 2+ ( foldr (+) [3,4] ) )
= 1+ ( 2+ ( 3+4 ) ) = 1+ ( 2+7 ) = 1+9 = 10

    (b)
foldr1 (-) [1,2,3,4]<-
= 1- ( foldr1 (-) [2,3,4] )
= 1- ( 2- ( foldr1 (-) [3,4] ) )
= 1- ( 2- ( 3-4 ) ) = 1- ( 2-(-1) ) = 1-3 = (-2)

foldr1 (-) [1,2,3,4]<-
= foldr1 (-) [1,2,(3-4)] = foldr1 (-) [1,2,(-1)]
= foldr1 (-) [1,(2-(-1))] = foldr1 (-) [1,3]
= 1 - 3 = -2

    (c)
foldr1 (/) [64,16,8,4]<-
= foldr1 (/) [64,16,(8/4)] = foldr1 (/) [64,16,2]
= foldr1 (/) [64,(16/2)] = foldr1 (/) [64,8]
= 64/8 = 8

    (d)
foldl1 (-) ->[1,2,3,4]
= foldl1 (-) [(1-2),3,4] = foldl1 (-) [(-1),3,4]
= foldl1 (-) [(-1 - 3),4] = foldl1 (-) [(-4),4]
= -4 - 4 = -8

-}


-- -- zadanie siódme a, last

foldedRightLast :: [a] -> a
foldedRightLast list = foldr1 (\ secondLast lastOne -> lastOne ) list

foldedLeftLast :: [a] -> a
foldedLeftLast list = foldl1 (\ first second -> second ) list


-- -- zadanie siódme b, head

foldedRightHead :: [a] -> a
foldedRightHead list = foldr1 (\ secondLast lastOne -> secondLast ) list

foldedLeftHead :: [a] -> a
foldedLeftHead list = foldl1 (\ first second -> first ) list


-- -- zadanie siódme c, max

foldedRightMax :: Ord a => [a] -> a
foldedRightMax list = foldr1
    (\ secondLast lastOne -> ( max secondLast lastOne ) ) list

foldedLeftMax :: Ord a => [a] -> a
foldedLeftMax list = foldl1 (\ first second -> ( max first second ) ) list
