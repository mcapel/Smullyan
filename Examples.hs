module Examples where
import Data
import Data.List 

honest = A 'H' 
liar = A 'L'
full = A 'F'
empty = A 'E'
innocent = A 'I'
guilty = A 'G'
warewolf = A 'W'
human = A 'U'
sane = A 'S'
insane = A 'N'

every,some,just1 :: Int -> (Int -> Prop) -> Prop
every i f = And [ f j | j <- [1..i] ]
some i f = Or [ f j | j <- [1..i] ]

-- just1 out of i is m
just1 i m = Or $ just1' i i m
   where
   just1' _ 0 _ = []
   just1' j i m = cnj:just1' j (i-1) m
      where 
       cnj = And (m i:[dualProp (m k) | k <- [1..j]\\[i] ])

-- i and j are of the same type p (e.g. both honest or liar)
same i j p = Or [And [p i,p j],
                 And [dualProp (p i),dualProp (p j)]]

infixr ==> 
(==>) :: Prop -> Prop -> Prop
a ==> b = Or [Not a,b]

------------------------------
-- Caskets
------------------------------
e67a = [(1,full 1),(2,empty 2),(3,empty 1)] :: [E]
f67a = [just1 3 full,just1 3 honest]
e67b = [(1,empty 2),(2,empty 2),(3,full 3)] :: [E]
f67b = [just1 3 full,Or [honest 1,honest 2,honest 3],Or [liar 1,liar 2,liar 3]]
e69a = [(1,full 1),(2,empty 2),(3,Or [just1 3 honest,every 3 liar])] :: [E]
f69a = [just1 3 full]
e69b = [(1,empty 1),(2,just1 2 honest)] :: [E]
f69b = [just1 3 full]
e69c = [(1,full 1),(2,full 2),(3,Or [And [liar 1,liar 2],And [liar 1,liar 3],And [liar 2,liar 3]])] :: [E]
f69c = [just1 3 full]
e70a = [(1,empty 1),(2,Or [And [honest 1,liar 2],And [liar 1,honest 2]])] :: [E]
f70a = [just1 2 full]
e70b = [(1,full 1),(2,empty 2),(3,just1 3 honest)] :: [E]
f70b = [just1 3 full]

ch5 = [(e67a,f67a),(e67b,f67b),(e69a,f69a),(e69b,f69b),(e69c,f69c),(e70a,f70a),(e70b,f70b)]

e131 = [(1,full 2 ==> honest 2),
        (2,full 2 ==> liar 1),
        (3,every 3 (\ j -> full j ==> liar j))] :: [E]
f131 = [just1 3 full]
ch9 = [(e131,f131)]

-- the lady and the tiger
lvt_f_1_1 = [just1 2 honest]
lvt_e_1_1 = [(1,And [full 1,empty 2]),(2, just1 2 full)] :: [E]
lvt_1_1 = (lvt_e_1_1,lvt_f_1_1)

lvt_f_1_2 = [same 1 2 honest]
lvt_e_1_2 = [(1,Or [full 1,full 2]),(2,empty 1)] :: [E]
lvt_1_2 = (lvt_e_1_2,lvt_f_1_2)

lvt_f_1_3 = [same 1 2 honest]
lvt_e_1_3 = [(1,Or [full 2,empty 1]),(2,full 1)] :: [E]
lvt_1_3 = (lvt_e_1_3,lvt_f_1_3)

lvt_f_1_4 = [full 1 ==> honest 1,
             empty 1 ==> liar 1,
             full 2 ==> liar 2,
             empty 2 ==> honest 2]
lvt_e_1_4 = [(1,And [full 1,full 2]),(2,And [full 1,full 2])] :: [E]
lvt_1_4 = (lvt_e_1_4,lvt_f_1_4)
lvt_e_1_5 = [(1,Or [full 1,full 2]),(2,full 1)] :: [E]
lvt_1_5 = (lvt_e_1_5,lvt_f_1_4)
lvt_e_1_6 = [(1,Or [And [full 1,full 2],And [empty 1,empty 2]]),(2,full 1)] :: [E]
lvt_1_6 = (lvt_e_1_6,lvt_f_1_4)
lvt_e_1_7 = [(1,Or [And [full 1,empty 2],And [empty 1,full 2]]),(2,full 1)] :: [E]
lvt_1_7 = (lvt_e_1_7,lvt_f_1_4)

lvt_f_1_9 = [just1 3 full,just1 3 honest]
lvt_e_1_9 = [(1,empty 1),(2,full 2),(3,empty 2)] :: [E]
lvt_1_9 = (lvt_e_1_9,lvt_f_1_9)

lvt_f_1_10 = [some 3 (A 'L'),every 3 (\ i -> full i ==> honest i)]
lvt_e_1_10 = [(1,empty 2),(2,empty 2),(3,empty 1)] :: [E]
lvt_1_10 = (lvt_e_1_10,lvt_f_1_10)
lvt_1 = [lvt_1_1,lvt_1_2,lvt_1_3,lvt_1_4,lvt_1_5,lvt_1_6,lvt_1_7,lvt_1_9,lvt_1_10]

lvt_e_2_1 = [(1,sane 2),(2,insane 1)] :: [E]
lvt_2_1 = (lvt_e_2_1,[])
------------------------------
-- Liar and honest
------------------------------
e28 = [(1,Or [liar 1,liar 2])] :: [E]
e29 = [(1,Or [liar 1,honest 2])] :: [E]
e31 = [(1,every 3 liar),(2,just1 3 honest)] :: [E]
e32 = [(1,every 3 liar),(2,just1 3 liar)] :: [E]
e33 = [(1,And [liar 1,honest 2])] :: [E]
e34 = [(1,liar 2),(2,same 1 3 honest)] :: [E]
e35 = [(1,same 2 3 honest)] :: [E]

ch3 = [(e28,[]),(e29,[]),(e31,[]),(e32,[]),(e33,[]),(e34,[]),(e35,[])]

e109 = [(1,honest 1 ==> honest 2)] :: [E]
e113 = [(1,honest 2 ==> liar 1)] :: [E]
e114 = [(1,guilty 3 ==> guilty 4),(2,Or [innocent 3,guilty 4])] :: [E]
e115 = [(1,honest 2),(2,honest 1 ==> honest 3)] :: [E]

ch8 = [(e109,[]),(e113,[]),(e114,[]),(e115,[])]
------------------------------
-- Warewolves
------------------------------
e88 = [(1,warewolf 3),(2,human 2),(3,Or [And [liar 1,liar 2],And [liar 1,liar 3],And [liar 2,liar 3]])] :: [E]
f88 = [just1 3 warewolf]
e89 = [(1,warewolf 1),(2,warewolf 2),(3,just1 3 honest)] :: [E]
f89 = [just1 3 warewolf]
e90 = [(1,some 3 honest),(2,some 3 liar)] :: [E]
f90 = [some 3 warewolf,every 3 (\ i -> honest i ==> human i)] -- CHECK
e91 = [(1,some 3 liar),(2,honest 3)] :: [E]
f91 = [just1 3 warewolf,every 3 (\ i -> warewolf i ==> honest i)]
e92 = [(1,some 3 liar),(2,warewolf 3)] :: [E]
f92 = [just1 3 warewolf,warewolf 1 ==> honest 1,warewolf 2 ==> honest 2,warewolf 3 ==> honest 3]

ch7 = [(e88,f88),(e89,f89),(e90,f90),(e91,f91),(e92,f92)]

e149 = [(1,Or [liar 1,warewolf 1])] :: [E]
e150 = [(1,And [liar 1,warewolf 1])] :: [E]
e151 = [(1,Not (And [honest 1,warewolf 1]))] :: [E]
e152 = [(1,Or [warewolf 1,warewolf 2]),(2,Or [liar 1,liar 2])] :: [E]
e153 = [(1,And [warewolf 1,warewolf 2]),(2,And [liar 1,liar 2])] :: [E]
e154 = [(1,And [liar 2,warewolf 2,Not (warewolf 1)]),(2,honest 1)] :: [E]
e155 = [(1,full 10),
        (2,Or [full 11,full 12]),
        (3,And [honest 1,honest 2]),
        (4,And [full 10,full 11]),
        (5,And [full 10,full 12]),
        (6,Or [honest 4, honest 5]),
        (7,honest 3 ==> honest 6),
        (8,And [honest 7,honest 8] ==> honest 1)] :: [E]
f155 = [Or [full 10, full 11, full 12, full 13]]
ch10 = [(e149,[]),(e150,[]),(e151,[]),(e152,[]),(e153,[]),(e154,[]),(e155,f155)]

e167 = [(1,Or [human 1,sane 1])] :: [E]
e168 = [(1,Not (And [human 1,sane 1]))] :: [E]
e169 = [(1,And [human 1,insane 1])] :: [E]
e171 = [(1,warewolf 1)] :: [E]
e172 = [(1,insane 1)] :: [E]
ch12 = [(e167,[]),(e168,[]),(e169,[]),(e171,[]),(e172,[])]

------------------------------
-- Inference
------------------------------
ch6a = [s71,s72,s74,s75,s76,s78,s79,s80,s81]

s71 = [Or [guilty 1,guilty 2,guilty 3],
       guilty 3 ==> guilty 1,
       guilty 2 ==> Or [guilty 3,guilty 1]]
s72 = [Or [guilty 1,guilty 2,guilty 3],
       innocent 3,
       guilty 1 ==> Or [guilty 3,guilty 2]]
s74 = [And [guilty 1,innocent 2] ==> guilty 3,
       guilty 3 ==> Or [guilty 2,guilty 1],
       guilty 1 ==> innocent 3,
       Or [guilty 1,guilty 2,guilty 3]]
s75 = [Or [guilty 1,guilty 2,guilty 3],
       guilty 1 ==> Or [guilty 2,guilty 3],     
       innocent 2 ==> innocent 3,
       innocent 3 ==> innocent 2,
       Or [And [guilty 1,guilty 2,innocent 3],And [guilty 1,innocent 2,guilty 3]]]  
s76 = [Or [guilty 1,guilty 2,guilty 3,guilty 4],
       innocent 1, 
       guilty 2 ==> Or [And [guilty 1,innocent 3,innocent 4],And [innocent 1,guilty 3,innocent 4],And [innocent 1,innocent 3,guilty 4]],
       guilty 3 ==> Or [And [guilty 1,guilty 2,innocent 4],And [guilty 1,innocent 2,guilty 4],And [innocent 1,guilty 2,guilty 4]]]     
s78 = [Or [innocent 1,guilty 2] ==> guilty 3,
       innocent 1 ==> innocent 3]
s79 = [Or [guilty 1,guilty 2,guilty 3],
       And [guilty 1,innocent 2] ==> guilty 3]
s80 = [And [guilty 1,guilty 2] ==> guilty 3,
       guilty 1 ==> Or [guilty 2,guilty 3],
       guilty 3 ==> guilty 4,
       innocent 1 ==> guilty 4]
s81 = [guilty 1 ==> guilty 2,
       guilty 2 ==> Or [guilty 3,innocent 1],
       innocent 4 ==> And [guilty 1,innocent 3],
       guilty 4 ==> guilty 1]

ch6b = [(e83,[]),(e84,[]),(e85,[])]
e83 = [(0,guilty 1),(0,Not (And [guilty 1,guilty 2]))] :: [E]
e84 = [(0,Or [guilty 1,guilty 2]),(0,innocent 1)] :: [E]
e85 = [(0,Or [innocent 1,guilty 2]),(0,guilty 1)] :: [E]
