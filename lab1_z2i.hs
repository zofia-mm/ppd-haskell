
{- pierwsze laboratoria -}
{- podpunkt I -}


fibonacci :: Integer -> Integer
fibonacci position = case position of
    1 -> 0
    2 -> 1
    x -> fibonacci (position-1) + fibonacci (position-2)


isNumberNthInFibo :: Integer -> Integer -> [Char]
isNumberNthInFibo number position =
    if ( number == fibonacci( position ) )
        then "tak"
        else "nie"
