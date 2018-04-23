module HW1 where

-- Exercise 1



data cmd =  pen mode
        |   moveto (pos, pos)
        |   def String pars cmd
        |   call String vals
        |   multiCMD cmd cmd

data mode = up | down
data pos = num Int | name String
data pars = multiPars String pars | singlePar String
data vals = multiVals Int vals | singleVal Int





