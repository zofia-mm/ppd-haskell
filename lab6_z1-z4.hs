

----------------------
-- zadanie pierwsze --
----------------------

simpleOperations :: IO()
simpleOperations = do
    putStr  "   podaj pierwszą liczbę > "
    x <- getLine
    putStr  "   podaj drugą liczbę > "
    y <- getLine
    putStr ( x ++ "+" ++ y ++ "=" ++ show ((read x::Int) + (read y::Int)) ++ ", " )
    putStr ( x ++ "-" ++ y ++ "=" ++ show ((read x::Int) - (read y::Int)) ++ ", " )
    putStr ( x ++ "*" ++ y ++ "=" ++ show ((read x::Int) * (read y::Int)) ++ "\n" )



--------------------
-- zadanie drugie --
--------------------

initials :: IO()
initials = do
    putStr  "   name > "
    name <- getLine
    putStr  "   surname > "
    surname <- getLine
    putStrLn ( [(head name)] ++ ". " ++ [(head surname)] ++ "." )



---------------------
-- zadanie trzecie --
---------------------

nwdANDnww :: IO()
nwdANDnww = do
    putStr      "   podaj pierwszą liczbę > "
    xString <- getLine
    let x = (read xString::Int)
    putStr      "   podaj drugą liczbę > "
    yString <- getLine
    let y = (read yString::Int)
    let nwdXY = nwd x y
    putStrLn   ("   NWD( " ++ xString ++ ", " ++ yString ++ " ) = "
                    ++ ( show nwdXY )  )
    putStrLn   ("   NWW( " ++ xString ++ ", " ++ yString ++ " ) = "
                    ++ ( show ( div (x*y) nwdXY ) )  )

nwd :: Int -> Int -> Int
nwd x y = if (mod x y) == 0
    then y
    else nwd y (mod x y)



---------------------
-- zadanie czwarte --
---------------------

pickShorterWord :: IO()
pickShorterWord = do
    putStr                  "   podaj pierwsze słowo > "
    wordOne <- getLine
    putStr                  "   podaj drugie słowo > "
    wordTwo <- getLine
    if (length wordOne) == (length wordTwo)
        then putStrLn       "   Oba słowa są tej samej długości!"
        else if (length wordOne) > (length wordTwo)
            then putStrLn  ("   Krótszym słowem jest: " ++ wordTwo ++ ".")
            else putStrLn  ("   Krótszym słowem jest: " ++ wordOne ++ ".")
