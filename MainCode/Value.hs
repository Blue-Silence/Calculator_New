module Value(
    value --接受一个操作符列表和Exp值，返回一个Exp值
)where

import DataType
import Tools
import Other_Function

--value=>value_1or3->before_value_2->value_2->value_1or3

type OptList=([ItemList Integer String (Exp->Exp->Exp)],([(String,(Exp->Exp))]),[ItemList Integer String (Exp->Exp->Exp)])
type List13=ItemList Integer String (Exp->Exp->Exp)

value :: OptList->Exp->Exp
value_1or3 :: OptList->List13->[Exp]->[Exp]
get_mutil_1 :: [Exp]->[Exp]->([Exp],[Exp])
trun_mutil_to_Exp :: [Exp]->Exp
before_value_2 :: [Exp]->[Exp]
value_2 :: OptList->[(String,(Exp->Exp))]->[Exp]->[Exp]
value_cycle :: OptList->(OptList->List13->[Exp]->[Exp])->[List13]->[Exp]->[Exp]

value all@(list1,list2,list3) (Exp exp)=car (value_cycle all (value_1or3) list3 (value_2  all list2 (before_value_2 (value_cycle all (value_1or3) list1 exp))))
value _ x=x

value_cycle _ _ [] result =result
value_cycle all proc (now:xs) result=value_cycle all proc xs (proc all now result)

value_1or3 _ _ []=[]
value_1or3 _ _ (x:[])=(x:[])
value_1or3 _ _ (x:y:[])=(x:y:[])
value_1or3 all listnow (x:(Opt opt):y:rest)
    |not (nullif opt_get) =(value_1or3 all listnow ([((car opt_get) (value all x) (value all y))]++rest))
    |otherwise =x:(Opt opt):(value_1or3 all listnow (y:rest))
        where opt_get=get_opt listnow opt
value_1or3 all listnow (x:xs)=x:(value_1or3 all listnow xs)

get_mutil_1 former []=(former,[])
get_mutil_1 former ((Opt x):xs)=get_mutil_1 (former++[(Opt x)]) xs
get_mutil_1 former (x:xs)=((former++[x]),xs)

trun_mutil_to_Exp (x:[])=x
trun_mutil_to_Exp (x:y)=Exp (x:(trun_mutil_to_Exp y):[])

before_value_2 []=[]
before_value_2 ((Opt opt):xs)=(Opt opt):exp:(before_value_2 rest)
                where (list,rest)=get_mutil_1 [] xs
                      exp=trun_mutil_to_Exp list
before_value_2 (x:xs)=x:(before_value_2 xs)

value_2 _ _ all@(x:[])=all
value_2 _ _ []=[]
value_2 all optlist (optexp@(Opt opt):x:xs)
    |not (nullif optget)=(value_2 all optlist ([((car optget) (value all x))]++xs))
    |otherwise=optexp:(value_2 all optlist (x:xs))
        where optget=get optlist opt 
value_2 all optlist (x:xs)=x:(value_2 all optlist xs)



