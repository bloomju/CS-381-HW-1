module HW1 where

-- Exercise 1

data Cmd =  Pen Mode
        |   MoveTo (Pos, Pos)
        |   Def String Pars Cmd
        |   Call String Vals
        |   MultiCMD Cmd Cmd

data Mode = Up | Down
data Pos = Num Int | Name String
data Pars = MultiPars String Pars | SinglePar String
data Vals = MultiVals Int Vals | SingleVal Int



