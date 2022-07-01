
import System.Random


imThinkingOfANumber :: IO()
imThinkingOfANumber = do
    number <- randomRIO (1::Int, 100)
    putStrLn    ""
    putStrLn    "   I'm thinking of a number between 1 and 100."
    putStrLn   ("   Would you care to have a guess?"
                ++ "Well hey, I'll give you 10 guesses.")
    putStrLn    ""
    guessLoop 1 11 number

        where

        guessLoop :: Int -> Int -> Int -> IO()
        guessLoop guessNo guessFull number = do
            if guessNo == guessFull
                then do
                    putStrLn   ("   You're outta guesses, it was "
                                ++ (show number) ++ ".")
                    putStrLn    ""
                    return ()
                else do
                    putGuessNo guessNo
                    finished <- getAndCompareNum number
                    if finished
                        then return()
                        else guessLoop (guessNo+1) guessFull number

        putGuessNo :: Int -> IO()
        putGuessNo guessNo = do
            putStr "   "
            case guessNo of
                1 -> putStr "1st"
                2 -> putStr "2nd"
                3 -> putStr "3rd"
                _ -> putStr ((show guessNo) ++ "th")
            putStr " guess > "

        getAndCompareNum :: Int -> IO Bool
        getAndCompareNum number = do
            guessStr <- getLine
            let guess = (read guessStr::Int)
            if guess == number
                then do
                    putStrLn        "   That's the number! Congrats!"
                    putStrLn        ""
                    return True
                else if guess < number
                    then do
                        putStrLn    "   Try higher."
                        putStrLn    ""
                        return False
                    else do
                        putStrLn    "   Try lower."
                        putStrLn    ""
                        return False
