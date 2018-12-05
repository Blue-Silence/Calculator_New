module Tools(
    next
   ,get_opt
   ,car
   ,cdr
   ,nullif
   ,ccar
)where

import DataType
import Other_Function

next (list1,list2)=((cdr list1),(cdr list2))

get_opt :: (Eq b)=>ItemList a b c->b->[c]
get_opt (ItemList _ list) opt=(get list opt)

car (x:y)=x
cdr (x:y)=y
nullif []=True
nullif x=False
ccar (x,_)=x

 