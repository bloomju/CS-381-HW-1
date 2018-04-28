-- CS381, Spring 2018
-- Assignment 1
-- Justin Bloom, David Jansen, Meghana Kolasani, Rushil Vora

module HW2 where

-- Exercise 1 --

type Prog = [Cmd]

data Cmd = LD Int
	 | ADD
	 | MULT
	 | DUP
	 deriving Show

type Stack = [Int]
type D = Stack -> Stack

semCmd :: Cmd -> D

semCmd (LD x) s = x:s
semCmd (ADD x:y:xs) = (x+y):xs
--semCmd (ADD) for fewer than 2 elements
semCmd (MULT x:y:xs) = (x*y):xs
--semCMD (MULT) for fewer than 2 elements
semCmd (DUP x:xs) = x:x:xs
--Error condition for DUP?

sem :: Prog -> D

sem (x:xs) s = sem xs (semCmd x s)
