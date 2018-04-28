-- CS381, Spring 2018
-- Assignment 2
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

{-
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
-}



-- Exercise 3 --
data LogoCmd = Pen Mode
                | MoveTo Int Int
                | Seq LogoCmd LogoCmd
                deriving Show
data Mode = Up | Down deriving Show

type State = (Mode, Int, Int)

type Line = (Int, Int, Int, Int)
type Lines = [Line]

semS :: LogoCmd -> State -> (State,Lines)

semS (Pen m) (_,x,y) = ((m,x,y), [])
semS (MoveTo a b) (Down, x, y)  = ((Down, a, b), [(x,y,a,b)])
semS (MoveTo a b) (Up, x, y) = ((Up, a, b), [])
semS (Seq c1 c2) (m, x, y) = ((m'', a', b'), l1++l2) 
    where   ((m'',a',b'), l1) = semS c1 (m',a,b) 
            ((m',a,b), l2) = semS c2 (m,x,y)
 



--sem' :: LogoCmd -> Lines


