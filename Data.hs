module Data where
import Data.List

data Prop = 
   A Char Int | Not Prop | Or [Prop] | And [Prop]
   deriving (Eq, Ord)

instance Show Prop where
   show (A c i) = [c] ++ show i
   show (Not p) = "-" ++ show p
   show (Or ps) = "[" ++ (intercalate " v " $ map show ps) ++ "]"
   show (And ps) = "[" ++ (intercalate " & " $ map show ps) ++ "]"

spellOutPair (i,p) = show i ++ " " ++ ": " ++ show p

spellOutList [] = []
spellOutList (e:es) = spellOutPair e ++ "\n" ++ spellOutList es

ind (A _ i) = [i]
ind (Not p) = ind p
ind (Or ps) = concatMap ind ps
ind (And ps) = concatMap ind ps

dual 'L' = 'H' -- Liar/Honest 
dual 'H' = 'L'
dual 'I' = 'G' -- Innocent/Guilty
dual 'G' = 'I'
dual 'W' = 'U' -- Human/Warewolf(/Monkey)
dual 'U' = 'W'
dual 'F' = 'E' -- Full/Empty
dual 'E' = 'F'
dual 'S' = 'N' -- Sane/Insane
dual 'N' = 'S'
dual x = x

dualProp (A c i) = A (dual c) i
dualProp _ = error "duality only defined for atoms"

type E = (Int,Prop)

