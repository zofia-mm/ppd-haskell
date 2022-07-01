
{- lab 5 -}
module Set
(
    intersect,
    union,
    difference,
    isSubsetOf,
    removeAllFromSet,
    setify
)
where


setify :: Eq a => [a] -> [a]
setify inputSet = innerSetify inputSet []

    where

        innerSetify :: Eq a => [a] -> [a] -> [a]
        innerSetify [] outputSet = outputSet
        innerSetify inputSet outputSet = if elem (head inputSet) outputSet
            then innerSetify (tail inputSet) outputSet
            else innerSetify (tail inputSet) ((head inputSet):outputSet)


intersect :: Eq a => [a] -> [a] -> [a]
intersect set1 set2 = innerIntersect (setify set1) set2 []

    where

        innerIntersect :: Eq a => [a] -> [a] -> [a] -> [a]
        innerIntersect [] set2 result = result
        innerIntersect set1 set2 result =
            if elem (head set1) set2 && not (elem (head set1) result)
                then innerIntersect (tail set1) set2 ((head set1):result)
                else innerIntersect (tail set1) set2 result


union :: Eq a => [a] -> [a] -> [a]
union set1 set2 = setify (set1++set2)


difference :: Eq a => [a] -> [a] -> [a]
difference [] set2 = []
difference set1 [] = set1
difference set1 set2 =
    difference (removeAllFromSet (head set2) set1) (tail set2)


removeAllFromSet :: Eq a => a -> [a] -> [a]
removeAllFromSet element set = innerRemove element set []

    where

        innerRemove :: Eq a => a -> [a] -> [a] -> [a]
        innerRemove element [] result = result
        innerRemove element set result = if (head set) == element
            then innerRemove element (tail set) result
            else innerRemove element (tail set) ((head set):result)


isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf [] set2 = True
isSubsetOf set1 set2 = if (elem (head set1) set2)
    then isSubsetOf (tail set1) set2
    else False













--
