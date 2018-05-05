-- CS381, Spring 2018
-- Assignment 3
-- Justin Bloom, David Jansen, Meghana Kolasani, Rushil Vora

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


rank :: Prog -> Rank -> Maybe Rank

-- Part B --

-- Exercise 2 --

data Shape = X
		| TD Shape Shape
		| LR Shape Shape
		deriving Show

type BBox = (Int, Int)

-- Part A --

-- Part B --

--Exercise 3 --

-- Part A --

-- Part B --

-- Part C --

-- Part D --




