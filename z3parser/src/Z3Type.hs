module Z3Type where

data Expr =
    ZFalse
    | ZTrue
    | ZInt Int
    | ZVar String
    | Select Expr Expr
    | Store Expr Expr Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | ZEq Expr Expr
    | Lt Expr Expr
    | Le Expr Expr
    | Gt Expr Expr
    | Ge Expr Expr
    | NOT Expr
    | AND Expr Expr
    | OR Expr Expr
    | Imp Expr Expr
    | Forall String Expr
    | Exists String Expr
    deriving (Eq,Show)

data Cmd =
    Skip
    | Assign String Expr
    | Update String Expr Expr
    | IF Expr Cmd Cmd
    | While Expr Expr Cmd
    | Seq Cmd Cmd
    deriving (Eq, Show)

data Var =
    NumVar String
    | ArrayVar String
    deriving(Eq,Show)

data Program = Program{
    vars :: [Var],
    pre :: Expr,    --事前 Q
    cmd :: Cmd,     --program P
    post :: Expr    --事後 R
}