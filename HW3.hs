-- CS381, Spring 2018
-- Assignment 3
-- Justin Bloom, Meghana Kolasani, Rushil Vora

module HW3 where

-- Exercise 1 --

type Prog = [Cmd]

data Cmd = LD Int
	 | ADD
	 | MULT
	 | DUP
	 | INC
	 | SWAP
	 | POP Int
	 deriving Show

type Stack = [Int]
type D = Stack -> Stack

semCmd :: Cmd -> D

semCmd (LD x) xs = x:xs
semCmd ADD (x:y:xs) = (x+y):xs
semCmd MULT (x:y:xs) = (x*y):xs
semCmd DUP (x:xs) = x:xs
semCmd INC (x:xs) = (x+1):xs
semCmd SWAP (x:y:xs) = (y:x:xs)
semCmd (POP x) xs = drop x xs

sem :: Prog -> D
sem [] s = s
sem (x:xs) s = sem xs (semCmd x s)

type Rank = Int
type CmdRank = (Int, Int)

-- Part A --

rankC :: Cmd -> CmdRank

rankC (LD x) = (0, 1)
rankC ADD  = (2, 1)
rankC MULT = (2, 1)
rankC DUP  = (1, 1)
rankC INC  = (1, 1)
rankC SWAP = (2, 2)
rankC (POP x) = (x, 0)

rankP :: Prog -> Maybe Rank

rankP xs = rank xs 0 -- Give rank 0 initially

rank :: Prog -> Rank -> Maybe Rank

rank [] r | r >= 0 = Just r
rank (x:xs) r | uf >=0 = rank xs (uf+m)
		where (n, m) = rankC x 
		      uf = r - n
rank _ _ = Nothing

-- Part B --

data Type = S Stack | Error
		    deriving Show

typeSafe :: Prog -> Bool
typeSafe t = (rankP t) /= Nothing

semStatTC :: Prog -> Type

semStatTC x | typeSafe x = S (sem x []) | otherwise = Error  

--Question: what is the new type of the function sem and why can the function defn be simpliied to have this type?
--Answer: The new type of the function sem is Prog -> D. The function definition can be simplified to have this type because the type errors are handled. 

-- Exercise 2 --

data Shape = X
		| TD Shape Shape
		| LR Shape Shape
		deriving Show

type BBox = (Int, Int)

-- Part A --

bbox :: Shape -> BBox

bbox X = (1,1)
bbox (TD s1 s2) | s1x >= s2x = (s1x, s1y+s2y) | s1x < s2x = (s2x, s1y + s2y)
                where (s1x, s1y) = bbox s1
		      (s2x, s2y) = bbox s2

bbox (LR s1 s2) | s1y >= s2y = (s1x+s2x, s1y) | s1y < s2y = (s2x+s1x, s2y)
                where (s1x, s1y) = bbox s1
                      (s2x, s2y) = bbox s2

bbox_ = error "you goofed"

-- Part B --


rect :: Shape -> Maybe BBox
rect X = Just(1,1)

rect (TD s1 s2) = case(s1x == s2x) of
		  True -> Just(s1x, s1y+s2y)
		  False -> Nothing
		  where (s1x, s1y) = bbox s1
		  	(s2x, s2y) = bbox s2

--Exercise 3 --

-- Part A --
-- (1) the type of f is [a] -> a -> [a]
--     the type of g is [a] -> b -> [b]
-- (2) f evaluates x as some array, so x is of type [a]. f can either return
--      [y] or x, which must be the same type. So the type of y must be a.
--     g evaluates x as some array, so x is of type [a]. g can either return
--      [y] or [], which means that y can be anything, so y is of type b.
-- (3) g is the more general type because x and y can be any type
-- (4) they have different types because static typing restricts x and y to be
--      related in type in f but not in y.
-- Part B --
h x y = x++[i | i <- (map snd y)] 

-- Part C --
k ff gg = ff (gg ff)
-- Part D --
-- This simply can not be done, if a function were to be defined at all it
-- can't just return anything, it has to return something. The return type of
-- a function can't be general, it must have some dependency on the type of 
-- something else, whether it's the type of something concrete or the type of
-- one of the variables. 
