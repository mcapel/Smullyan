module Prop where
import Data.List 
import Debug.Trace
import qualified Data.IntMap.Strict as M

data Prop = 
   A Int | Not Prop | Or [Prop] | And [Prop] | T | F
   deriving (Eq, Ord)

instance Show Prop where
   show (A x) = show x
   show (Not p) = "-" ++ show p
   show (Or ps) = "v(" ++ show' ps ++ ")"
   show (And ps) = "&(" ++ show' ps ++ ")"

show' [] = ""
show' [a] = show a
show' (p:ps) = show p ++ "," ++ show' ps

a = A 1
b = A 2
c = A 3
d = A 4
e = A 5
f = A 6
l = A 7
m = A 8
n = A 9

-- All A are B
uni a b = Or [Not a,b]
-- Some a are B
exi a b = And [a,b]
-- No A are B
uni' a b = Or [Not a,Not b]
-- Some A are not B
exi' a b = And [a,Not b]

(==>) :: [Prop] -> Prop -> Prop
ps ==> p = Or [Not (And ps),p]

clean (And xs) = clean' (And []) [] xs
clean (Or xs) = clean' (Or []) [] xs
clean (Not x) = Not (clean x)
clean p = p

clean' z@(And []) (And ys:yss) xs = clean' z yss (ys++xs)
clean' z@(And []) ys (x:xs) = clean' z (clean x:ys) xs
clean' z@(Or []) (Or ys:yss) xs = clean' z yss (ys++xs)
clean' z@(Or []) ys (x:xs) = clean' z (clean x:ys) xs
clean' (And []) ys [] = And ys
clean' (Or []) ys [] = Or ys

normalize :: Prop -> Prop
normalize p = 
   let 
     p1 = inNeg p
     p2 = distr p1
     p3 = clean p2
   in p3
-------------------------------------------
-------------------------------------------
-- -(A & B) => -A v -B
-- -(A v B) => -A & -B
inNeg :: Prop -> Prop
inNeg (And ps) = And $ map inNeg ps
inNeg (Or ps) = Or $ map inNeg ps
inNeg (Not p) =
   case inNeg p of
      (Not p') -> p'
      (And xs) -> Or [ inNeg (Not x) | x <- xs ] 
      (Or xs) -> And [ inNeg (Not x) | x <- xs ] 
      p' -> Not p'
inNeg x = x
-------------------------------------------
-------------------------------------------
-- Z v (X & Y) => (Z v X) & (Z v Y)
-- (X & Y) v Z => (Z v X) & (Z v Y)

distr (And ps) = And $ map distr ps
distr (Or ps) = distr' [] ps
   where
     distr' (q:qs) (And ps:pss) = distr' qs (And ps':pss)
         where ps' = [ distr $ (Or [q,p]) | p <- ps ]
     distr' (And qs:qss) (p:ps) = distr' (p:qss) (And qs:ps)
     distr' qs (p:ps) = distr' (p:qs) ps
     distr' [x] [] = x
     distr' qs [] = Or qs
distr p = p
-------------------------------------------
-- sat -- no normalization
-------------------------------------------
sat v p = map M.toList vss'
   where
    vss' = filterMaps vss
    vss = sat' [M.empty] v [p]

sat' ts v [] = ts
sat' ts v (A c:as) = 
   sat' ts' v as
    where ts' = update ts c v
sat' ts v (Not p:as) = 
   sat' ts' v as
    where ts' = sat' ts (not v) [p]
sat' ts True (And ps:as) = sat' ts True (ps++as)
sat' ts False (Or ps:as) = sat' ts False (ps++as) 
sat' ts False (And ps:as) = 
   concat [ sat' ts False (p:as) | p <- ps ]
sat' ts True (Or ps:as) = 
   concat [ sat' ts True (p:as) | p <- ps ]
-------------------------------------------
-- sat1 -- normalization
-------------------------------------------
sat1 v p = map M.toList vss'
   where
    p' = normalize p
    vss' = filterMaps vss
    vss = sat1' [M.empty] v [p']

sat1' ts v [] = ts
sat1' ts v (A c:as) = 
   sat1' ts' v as
    where ts' = update ts c v
sat1' ts v (Not (A c):as) = 
   sat1' ts' v as
    where ts' = update ts c (not v)
sat1' ts True (And ps:as) = sat1' ts True (ps++as)
sat1' ts False (Or ps:as) = sat1' ts False (ps++as) 
sat1' ts False (And ps:as) = 
   concat [ sat1' ts False (p:as) | p <- ps ]
sat1' ts True (Or ps:as) = 
   concat [ sat1' ts True (p:as) | p <- ps ]
-------------------------------------------
-------------------------------------------
filterMaps ts = filterMaps' [] ts
   where
   filterMaps' ts [] = ts
   filterMaps' ts' (t:ts) | or [ M.isSubmapOf t' t | t' <- ts++ts' ] = filterMaps' ts' ts
                          | otherwise = filterMaps' (t:ts') ts

update [] _ _ = []
update (t:ts) i v =
   let insertLookup kx x t = M.insertLookupWithKey (\_ a _ -> a) kx x t
   in case insertLookup i v t of
      (Nothing, t') -> t': update ts i v
      (Just v',t') | v==v' -> t': update ts i v
                   | otherwise -> update ts i v

