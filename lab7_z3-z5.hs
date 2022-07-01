

-- --------------- --
-- zadanie trzecie --
-- --------------- --

bubbleSort :: Ord a => [a] -> [a]
bubbleSort list = innerBubble list [] 0
    where
    innerBubble :: Ord a => [a] -> [a] -> Int -> [a]
    innerBubble [] alteredList 0 =
        alteredList
    innerBubble [] alteredList inversions =
        innerBubble alteredList [] 0
    innerBubble (headL:[]) alteredList inversions =
        innerBubble [] (alteredList++[headL]) inversions
    innerBubble inputList alteredList inversions =
        if (head inputList) > ((head.tail) inputList)
            then innerBubble
                    ( (head inputList):((tail.tail) inputList) )
                    ( alteredList ++ [(head.tail) inputList] )
                    ( inversions+1 )
            else innerBubble
                    ( tail inputList )
                    ( alteredList ++ [head inputList] )
                    inversions



-- --------------- --
-- zadanie czwarte --
-- --------------- --

compareStrings :: IO()
compareStrings = do
    putStr "podaj pierwsze słowo > "
    word1 <- getLine
    putStr "podaj drugie słowo > "
    word2 <- getLine
    innerCS word1 word2

        where

        innerCS :: String -> String -> IO()
        innerCS [] word2 = putStrLn (concat (replicate (length word2) "-") )
        innerCS word1 [] = putStrLn (concat (replicate (length word1) "-") )
        innerCS word1 word2 = if (head word1) == (head word2)
            then do
                putStr [(head word1)]
                innerCS (tail word1) (tail word2)
            else do
                putStr "-"
                innerCS (tail word1) (tail word2)



-- ------------- --
-- zadanie piąte --
-- ------------- --

file :: IO()
file = do
    putStrLn "input something below"
    stuffToWrite <- getLine
    writeFile "file.txt" stuffToWrite
    readFromFile <- readFile "file.txt"
    putStrLn ("this many char were written to file.txt > "
             ++( (show.length) readFromFile ))



















--
