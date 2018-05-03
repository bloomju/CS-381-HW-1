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
	 | DEF String Prog
	 | CALL String
	 deriving Show

type Stack = [Int]
type D = Stack -> Stack

semCmd :: Cmd -> D

semCmd (LD x) xs = x:xs
semCmd ADD (x:y:xs) = (x+y):xs
semCmd MULT (x:y:xs) = (x*y):xs
semCmd DUP (x:xs) = x:xs

sem :: Prog -> D
sem [] s = s
sem (x:xs) s = sem xs (semCmd x s)

test :: Prog -> Stack
test x = sem x []
test1 = [LD 3, DUP, ADD, DUP, MULT]
test2 = [LD 3, ADD] 
test3 = [] 

-- Exercise 2 --

-- Part A --

-- Extended the syntax in exercise 1

-- Part B --


--type Prog = [Cmd]

type Macros = [(String, Prog)]
type PState = (Macros, Stack)
type P = PState -> PState

-- Part C --

semCmd2 :: Cmd -> P

semCmd2 (LD x) (m, xs)     = (m, x:xs)
semCmd2 (ADD)  (m, x:y:xs) = (m, (x+y):xs)
semCmd2 (MULT) (m, x:y:xs) = (m, (x*y):xs)
semCmd2 (DUP)  (m, x:xs)   = (m, x:xs)
semCmd2 (DEF n p) (m, xs)  = ((n,p):m,xs)
semCmd2 (CALL n) (m, xs) =  (m, sem pp xs) where _:(n,pp):_ = m

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
 
sem' :: LogoCmd -> Lines

sem' cc = ll where (_,ll) = semS cc (Up, 0,0)
