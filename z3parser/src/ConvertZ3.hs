module ConvertZ3 where

import Control.Applicative
import Z3Type

convertVar :: [Var] -> [String]
convertVar = map (addParen.appendV.f) where
    addParen str = "(" ++ str ++ ")"
    appendV = (++) "declare-var "
    f (NumVar v) = v ++ " Int"
    f (ArrayVar v) = v ++ " (Array Int Int)"

convertCmd :: Cmd -> String
convertCmd cmd
    | Skip = ""
    | IF a b c = ""
    | WHI