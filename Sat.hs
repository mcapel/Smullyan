module Sat where
import Data.List
import Data 
import Aux

eval :: Int -> [E] -> [Prop] -> [Assignment]
eval k ps qs = pruneSubsumed (nub (map sort [ v | Just v <- eval' k ps qs ]))

eval' :: Int -> [E] -> [Prop] -> [Maybe Assignment]
eval' k [] [] = [Just []]
eval' k ((i,p):ps) [] = 
   nub $ ts' ++ fs'
   where
    vs = eval' k ps []
    ts' = [ consistentMapUnion t v | t <- checkLiar k i True p, Just v <- vs ]
    fs' = [ consistentMapUnion f v | f <- checkLiar k i False p, Just v <- vs ]
eval' k ps (q:qs) = 
   nub [ consistentMapUnion t v | t <- satisfy [[]] True [q], Just v <- vs ]
   where
    vs = eval' k ps qs

-- Speaker i states p as v
checkLiar :: Int -> Int -> Bool -> Prop -> [Assignment]
checkLiar k i v p = satisfy [[ (i,v') ] | v' <- toLabel k v ] v [p]
   where 
    toLabel 1 True = ["SU","NW"] -- play with sane vs insane
    toLabel 1 False = ["NU","SW"]
    toLabel 0 True = ["H"] -- play with honest vs liars
    toLabel 0 False = ["L"]

satisfy :: [Assignment] -> Bool -> [Prop] -> [Assignment]
satisfy ts _ [] = ts
satisfy ts True (A p c:as) = 
   satisfy ts' True as
    where ts' = updateAssignments (c,[p]) ts
satisfy ts False (A p c:as) = 
   satisfy ts' False as
    where ts' = updateAssignments (c,[dual p]) ts
satisfy ts v (Not p:as) = 
   satisfy ts' v as
    where ts' = satisfy ts (not v) [p]
satisfy ts True (And ps:as) = 
   satisfy ts True (ps++as)
satisfy ts False (Or ps:as) = 
   satisfy ts False (ps++as) 
satisfy ts False (And ps:as) = 
   concat [ satisfy ts False (p:as) | p <- ps ]
satisfy ts True (Or ps:as) = 
   concat [ satisfy ts True (p:as) | p <- ps ]

