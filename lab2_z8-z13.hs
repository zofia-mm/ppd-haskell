
-- -- zadanie ósme

listThisNTimes this times = buildList this times []
    where
        buildList this 0 list = list
        buildList this times list = buildList this (times-1) (list++[this])


-- -- zadanie dziewiąte

isPalindrome [] = True
isPalindrome list = if ( length list ) == 1
    then True
    else if ( head list ) == ( last list )
        then isPalindrome ( init ( tail list ) )
        else False


-- -- zadanie dziesiąte

removeNthFromList n list = if n > ( length list )
    then list
    else ( take (n-1) list ) ++ ( drop n list )


-- -- zadanie jedenaste

doesListAIcludeListB [] listB = True
doesListAIcludeListB listA listB = if ( head listA ) == ( head listB )
    then doesListAIcludeListB ( tail listA ) ( tail listB )
    else False

doesListIncludeElements list [] = True
doesListIncludeElements list elements = if ( elem ( head elements ) list )
    then doesListIncludeElements list (tail elements)
    else False


-- -- zadanie dwunaste

reversePairs :: [ (a,b) ] -> [ (b,a) ]
reversePairs listOfPairs = map reversePair listOfPairs
    where
        reversePair :: (a,b) -> (b,a)
        reversePair pair = ( snd pair, fst pair )


-- -- zadanie trzynaste a

power :: Num a => a -> Integer -> a
power a n = acumulatePower a n 1
    where
        acumulatePower :: Num a => a -> Integer -> a -> a
        acumulatePower a 0 summer = summer
        acumulatePower a n summer = acumulatePower a (n-1) (summer*a)


-- -- zadanie trzynaste b

nthInSequence :: Integer -> Integer
nthInSequence 1 = 3
nthInSequence 2 = 1
nthInSequence n = sequenceGo 1 3 (n-2)
    where
        sequenceNext aMinus1 aMinus2 = ( ( 2 * aMinus1 ) - aMinus2 )
        sequenceGo aMinus1 aMinus2 0 = aMinus1
        sequenceGo aMinus1 aMinus2 n = sequenceGo
            ( sequenceNext aMinus1 aMinus2 )
            aMinus1
            ( n - 1 )














--
