

-- ---------------- --
-- zadanie pierwsze --
-- ---------------- --

countElsInList :: Eq a => [a] -> [(a, Int)]
countElsInList list = innerCEIN list []

    where

    innerCEIN :: Eq a => [a] -> [(a, Int)] -> [(a, Int)]
    innerCEIN [] result = result
    innerCEIN list result =
        innerCEIN
            ( removeElFromList (head list) (tail list) )
            ( result ++ [( head list, countElInList (head list) (tail list) )] )

    countElInList :: Eq a => a -> [a] -> Int
    countElInList el list = innerCount el list 1
        where
        innerCount :: Eq a => a -> [a] -> Int -> Int
        innerCount el [] counter = counter
        innerCount el (hlist:tlist) counter = if el == hlist
            then innerCount el tlist (counter+1)
            else innerCount el tlist counter

    removeElFromList :: Eq a => a -> [a] -> [a]
    removeElFromList el list = innerRemove el list []
        where
        innerRemove :: Eq a => a -> [a] -> [a] -> [a]
        innerRemove el [] resultList = resultList
        innerRemove el (hlist:tlist) resultList = if hlist == el
            then innerRemove el tlist resultList
            else innerRemove el tlist (resultList++[hlist])



-- -------------- --
-- zadanie drugie --
-- -------------- --


powerList :: [a] -> [[a]]
powerList list = innerPower list [[]]

    where

    innerPower :: [a] -> [[a]] -> [[a]]
    innerPower [] power = power
    innerPower (hlist:tlist) power =
        innerPower
            tlist
            ( power ++ (addFirst hlist power []) )

    addFirst :: a -> [[a]] -> [[a]] -> [[a]]
    addFirst el [] resultList = resultList
    addFirst el (hEl:tList) resultList = addFirst el tList (resultList++[el:hEl])




















--
