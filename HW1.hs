module HW1 where

-- Exercise 1

data Cmd =  Pen Mode
        |   MoveTo (Pos, Pos)
        |   Def String Pars Cmd
        |   Call String Vals
        |   MultiCmd Cmd Cmd

data Mode = Up | Down
data Pos = Num Int | Name String
data Pars = MultiPars String Pars | SinglePar String
data Vals = MultiVals Int Vals | SingleVal Int

vector (x1, y1) (x2, y2) = 
                Def "vector" (MultiPars "p1" (SinglePar "p2"))
                (MultiCmd (Pen Up) 
                (MultiCmd (MoveTo (Num x1, Num y1))
                (MultiCmd (Pen Down)
                (MoveTo (Num x2, Num y2)))))

steps :: Int -> Cmd

steps 0 = MultiCmd (Pen Up) (MoveTo (Num 0, Num 0))

steps n = 
            MultiCmd (Pen Up)
            (MultiCmd (MoveTo (Num n, Num n))
            (MultiCmd (MoveTo (Num (n-1), Num n))
            (MultiCmd (MoveTo(Num (n-1), Num (n-1)))
            (steps (n-1)) )))






