module Value(valueall)where

import DataType
import Other_Function
import Tools
type Opttype=[Exp]->[Exp]->(Exp,[Exp],[Exp])

value :: [(String,Opttype)]->[Exp]->[Exp]->[Exp]
value _ before []=reverse before
value optlist before ((Opt x):rest)
    |notnull opt=let result=(car opt) before rest
                 in value optlist ((st result):(nd result)) (rd result)
    |otherwise=value optlist ((Opt x):before) rest
        where opt=get optlist x
value optlist before (x:rest)=value optlist (x:before) rest



valueall :: [[(String,Opttype)]]->Exp->Exp
valueall list (Exp exp)=car (foldl (valuestart) expnow list)
                        where valuestart input listnew=value listnew [] input
                              expnow=map (valueall list) exp