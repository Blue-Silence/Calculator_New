module Tools(
    next
   ,get_opt
   ,car
   ,cdr
   ,notnull
   ,ccar
   ,clothesOff
   ,st
   ,nd
   ,rd
)where

import DataType
import Other_Function

next (list1,list2)=((cdr list1),(cdr list2))

get_opt :: (Eq b)=>ItemList a b c->b->[c]
get_opt (ItemList _ list) opt=(get list opt)

car (x:y)=x
cdr (x:y)=y
notnull []=False
notnull x=True
ccar (x,_)=x

st (x,_,_)=x
nd (_,x,_)=x
rd (_,_,x)=x

clothesOff :: ItemList a b c->[(b,c)]
clothesOff (ItemList _ x)=x
 