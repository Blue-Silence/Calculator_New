module Transfer (
    transfer --第一个参数为运算符表，返回Exp类型
     )where

import DataType
import Tools
import Other_Function
data Exps = Opts [Char] | Exps [Exps] | Nums Double | Begins | Ends
    deriving (Show,Eq)

transfer list x=(ccar (transferf [] (stringsToExpsList (words x) list)))
    
transfer1 :: String->[String]->[Exps]
transfer1 ('(':xs) list=Begins:(transfer1 xs list)
transfer1 (')':xs) list=Ends:(transfer1 xs list)
transfer1 [] _ =[]
transfer1 input list 
        |notnull nums=(Nums ((ccar . car) nums)) :(transfer1 ((ccdr . car) nums) list)
        |otherwise=(Opts (ccar opt)):(transfer1 (ccdr opt) list)
            where opt=getStringPairInString input list
                  nums=(reads input :: [(Number,String)])
                  ccar (x,_)=x
                  ccdr (_,y)=y
 
stringsToExpsList :: [String]->[String]->[Exps]
stringsToExpsList [] _  =[]
stringsToExpsList (x:xs) list =(transfer1 x list)++(stringsToExpsList xs list)

transferf :: [Exp]->[Exps]->(Exp,[Exps])
transferf [] ((Opts x):[]) =((Opt x),[])
transferf [] ((Nums x):[]) =((Num x),[])
transferf fromer []=((Exp fromer),[])
transferf former (Ends:rest)=((Exp former),rest)
transferf fromer (Begins:x)=transferf (fromer++[exp]) rest
                        where (exp,rest)=transferf [] x
transferf former (x:rest)=transferf (former++[(ccar (transferf [] [x]))]) rest



