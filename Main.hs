module Main where
import Control.Monad
import Data
import Examples
import Sat

evalPrint k ps qs = 
   do
    putStrLn "ASSUMPTIONS:" 
    putStr $ concatMap (\ x -> show x ++ "\n") qs
    putStrLn "STATEMENTS:" 
    putStr $ spellOutList ps
    putStrLn "CONCLUSIONS:" 
    putStrLn $ show (eval k ps qs)
    putStrLn "==================="

inferPrint ps = 
   do
    putStrLn "ASSUMPTIONS:" 
    putStr $ concatMap (\ x -> show x ++ "\n") ps
    putStrLn "CONCLUSIONS:" 
    putStrLn $ show (eval 0 [] ps)
    putStrLn "==================="


main =
    do
    putStrLn "Abbreviations:"
    putStrLn "H = honest == not liar"
    putStrLn "L = liar == not honest"
    putStrLn "F = full == not empty"
    putStrLn "E = empty == not full"
    putStrLn "G = guilty == not innocent"
    putStrLn "I = innocent == not guilty"
    putStrLn "S = sane == not insane"
    putStrLn "N = insane == not sane"
    putStrLn "U = human == not warewolf (or monkey or vampire)"
    putStrLn "W = warewolf (or monkey or vampire) == not human"
    putStrLn "Int i : [Prop p] = speaker i says that p"
    mapM (\ (s,f) -> evalPrint 0 s f) (ch3 ++ ch5 ++ lvt_1 ++ ch6b ++ ch7 ++ ch9 ++ ch10)
    >>
    mapM (\ (s,f) -> evalPrint 1 s f) ch12
    >>
    mapM inferPrint ch6a
   
