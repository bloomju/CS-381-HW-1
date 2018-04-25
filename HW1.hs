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

vector :: (Pos, Pos) -> (Pos, Pos) -> Cmd

vector (x1, y1) (x2, y2) = 
                Def "vector" (MultiPars "p1" (SinglePar "p2"))
                (MultiCmd (Pen Up) 
                (MultiCmd (MoveTo (x1, y1))
                (MultiCmd (Pen Down)
                (MoveTo (x2, y2) ))))

steps :: Int -> Cmd

steps 0 = MultiCmd (Pen Up) (MoveTo (Num 0, Num 0))

steps n = 
            MultiCmd (Pen Up)
            (MultiCmd (MoveTo (Num n, Num n))
            (MultiCmd (Pen Down)
            (MultiCmd (MoveTo (Num (n-1), Num n))
            (MultiCmd (MoveTo(Num (n-1), Num (n-1)))
            (steps (n-1)) ))))

-- Exercise 3

-- Part (A).  Apply Multiply[Apply Negate[Apply Add [Num 3, Num 4]], Num 7]


--Part (B).  One of the disadvantages of the second form of syntax is that it is less clear how many expressions you can use with each operation.
--For example in the first abstract syntax we know that each operation expects exactly two different expressions, while for the second abstract syntax we can use as
--many expressions as we want because it is using a list rather than a defined amount of expressions.  Not only could you use more than 2 expressions but you could 
--also use less than two expressions, which doesn't make sense when attempting to multiply a single Int. This causes a bit of confusion when attempting to represent
--expressions using this syntax.  Another disadvantage of the second abstract syntax is that it is not as easy to use as the first abstract syntax in order to write expressions 
--because it ends up using two seperate data type.  This makes it so we have to use two data types in each expression rather than one which makes representing an entire 
--expression in this syntax.  The only advantage from using the second data type is that you are not restricted to only two expressions per operator.  

data Expr = N Int
            | Plus Expr Expr
            | Times Expr Expr
            | Neg Expr

data Op = Add | Multiply | Negate

data Exp = NN Int
        | Apply Op [Exp]

translate :: Expr -> Exp 

translate (N x) = (NN x)

translate (Plus e1 e2) = Apply Add [translate e1,translate e2]

translate (Times e1 e2) = Apply Multiply [translate e1, translate e2]

translate (Neg e1) = Apply Negate [translate e1]
