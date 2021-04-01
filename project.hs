import Prelude as P
import Data.Ix
import Data.Array
import System.Random
import System.Exit
import System.Process

--a = [ "X | X | X", "---------", "X | X | X", "---------", "X | X | X" ]

--ar = array (1,100) ((1,1) : [(i, i * a!(i-1)) | i <- [2..100]])

--b = [ a1 ++ " | " ++ b1 ++ " | " ++ c1, "---------", a2 ++ " | " ++ b2 ++ " | " ++ c2, "---------", a3 ++ " | " ++ b3 ++ " | " ++ c3 ]

checkWinner a1 a2 a3 b1 b2 b3 c1 c2 c3 | (a1 == a2) && (a2 == a3) && (a3 /= "  ") = a1
                                       | (b1 == b2) && (b2 == b3) && (b3 /= "  ") = b1
                                       | (c1 == c2) && (c2 == c3) && (c3 /= "  ") = c1
                                       | (a1 == b1) && (b1 == c1) && (c1 /= "  ") = a1
                                       | (a2 == b2) && (b2 == c2) && (c2 /= "  ") = a2
                                       | (a3 == b3) && (b3 == c3) && (c3 /= "  ") = a3
                                       | (a1 == b2) && (b2 == c3) && (c3 /= "  ") = b2
                                       | (a3 == b2) && (b2 == c1) && (c1 /= "  ") = b2
                                       | (a1 /= "  ") && (a2 /= "  ") && (a3 /= "  ") && (b1 /= "  ") && (b2 /= "  ") && (b3 /= "  ") && (c1 /= "  ") && (c2 /= "  ") && (c3 /= "  ") = "Draw"  
                                       | otherwise = "Nothing"



printa [] = return()
printa (x:xs) = do
    putStrLn (x)
    printa (xs)

-- :! cls

search a1 a2 a3 b1 b2 b3 c1 c2 c3 = do  gen <- newStdGen
                                        if ((check a1 a2 a3 b1 b2 b3 c1 c2 c3 (take 1 (randomRs ('1','9') gen))) == "  ")
                                            then return (take 1 (randomRs ('1','9') gen))
                                            else search a1 a2 a3 b1 b2 b3 c1 c2 c3  
                                                    
botHod a1 a2 a3 b1 b2 b3 c1 c2 c3 hod bot player =   do putStrLn("\nBot's hodnt\n\n")
                                                        s2 <- search a1 a2 a3 b1 b2 b3 c1 c2 c3
                                                        case s2 of
                                                                "1" -> game a1 a2 bot b1 b2 b3 c1 c2 c3 hod bot player
                                                                "2" -> game a1 a2 a3 b1 b2 bot c1 c2 c3 hod bot player
                                                                "3" -> game a1 a2 a3 b1 b2 b3 c1 c2 bot hod bot player
                                                                "4" -> game a1 bot a3 b1 b2 b3 c1 c2 c3 hod bot player
                                                                "5" -> game a1 a2 a3 b1 bot b3 c1 c2 c3 hod bot player
                                                                "6" -> game a1 a2 a3 b1 b2 b3 c1 bot c3 hod bot player
                                                                "7" -> game bot a2 a3 b1 b2 b3 c1 c2 c3 hod bot player
                                                                "8" -> game a1 a2 a3 bot b2 b3 c1 c2 c3 hod bot player
                                                                "9" -> game a1 a2 a3 b1 b2 b3 bot c2 c3 hod bot player
                                                        game a1 a2 a3 b1 bot b3 c1 c2 c3 (hod) bot player 


check a1 a2 a3 b1 b2 b3 c1 c2 c3 c = case c of
                                            "1" -> a3
                                            "2" -> b3
                                            "3" -> c3
                                            "4" -> a2
                                            "5" -> b2
                                            "6" -> c2
                                            "7" -> a1
                                            "8" -> b1
                                            "9" -> c1

start = do  putStrLn "choose your symbol (X or O)"
            s1 <- getLine
            case s1 of
                "0" -> game "  " "  " "  " "  " "  " "  " "  " "  " "  " (-1) "X" "O" 
                "O" -> game "  " "  " "  " "  " "  " "  " "  " "  " "  " (-1) "X" "O" 
                "o" -> game "  " "  " "  " "  " "  " "  " "  " "  " "  " (-1) "X" "O" 
                "X" -> game "  " "  " "  " "  " "  " "  " "  " "  " "  " 0 "O" "X" 
                "x" -> game "  " "  " "  " "  " "  " "  " "  " "  " "  " 0 "O" "X" 
game a1 a2 a3 b1 b2 b3 c1 c2 c3 hod bot player = do if (hod == -1)
                                                        then do botHod a1 a2 a3 b1 b2 b3 c1 c2 c3 (hod+1) bot player
                                                            else
                                                                if (hod `mod` 2 == 0)
                                                                    then do 
                                                                            system "cls"
                                                                            printa ["\n \n\n\nyour turn\n", a1 ++ " | " ++ b1 ++ " | " ++ c1, "------------", a2 ++ " | " ++ b2 ++ " | " ++ c2, "------------", a3 ++ " | " ++ b3 ++ " | " ++ c3 ++ "\n"]
                                                                            case (checkWinner a1 a2 a3 b1 b2 b3 c1 c2 c3) of
                                                                                "X" ->   do putStrLn ("winner - X \nwaiting command\n")
                                                                                            s1 <- getLine
                                                                                            case s1 of
                                                                                                "exit" -> exitFailure
                                                                                                "restart" -> start
                                                                                                "start" -> start
                                                                                "O" ->   do putStrLn ("winner - O \nwaiting command\n")
                                                                                            s1 <- getLine
                                                                                            case s1 of
                                                                                                "exit" -> exitFailure
                                                                                                "restart" -> start
                                                                                                "start" -> start
                                                                                "Draw" ->  do putStrLn ("Draw \nwaiting command\n")
                                                                                              s1 <- getLine
                                                                                              case s1 of
                                                                                                  "exit" -> exitFailure
                                                                                                  "restart" -> start
                                                                                                  "start" -> start
                                                                                "Nothing" -> do
                                                                                                s1 <- getLine
                                                                                                case s1 of
                                                                                                    "exit" -> exitFailure
                                                                                                    "restart" -> start
                                                                                                    "a1" -> game player a2 a3 b1 b2 b3 c1 c2 c3 (hod+1) bot player                                                   
                                                                                                    "a2" -> game a1 player a3 b1 b2 b3 c1 c2 c3 (hod+1) bot player                                                   
                                                                                                    "a3" -> game a1 a2 player b1 b2 b3 c1 c2 c3 (hod+1) bot player                                                   
                                                                                                    "b1" -> game a1 a2 a3 player b2 b3 c1 c2 c3 (hod+1) bot player                                                   
                                                                                                    "b2" -> game a1 a2 a3 b1 player b3 c1 c2 c3 (hod+1) bot player                                                   
                                                                                                    "b3" -> game a1 a2 a3 b1 b2 player c1 c2 c3 (hod+1) bot player                                                   
                                                                                                    "c1" -> game a1 a2 a3 b1 b2 b3 player c2 c3 (hod+1) bot player                                                   
                                                                                                    "c2" -> game a1 a2 a3 b1 b2 b3 c1 player c3 (hod+1) bot player                                                   
                                                                                                    "c3" -> game a1 a2 a3 b1 b2 b3 c1 c2 player (hod+1) bot player                                                   
                                                                    else botHod a1 a2 a3 b1 b2 b3 c1 c2 c3 (hod+1) bot player

main = start
