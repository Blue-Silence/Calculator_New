module ValueMain(valueMain)where

import Transfer
import Value
import DataType

type OptList=([ItemList Integer String (Exp->Exp->Exp)],([(String,(Exp->Exp))]),[ItemList Integer String (Exp->Exp->Exp)])

valueMain optlist str=value optlist (((transfer . getOptStr) optlist) str)

getName :: [(a,b)]->[a]
getName=foldl (\former (x,_)->(x:former)) [] 

getOptStr :: OptList->[String]
getOptStr ([],[],[])=[]
getOptStr ([],[],((ItemList _ x):xs))=(getName x)++(getOptStr ([],[],xs))
getOptStr (((ItemList _ x):xs),y,rest)=(getName y)++(getName x)++(getOptStr (xs,[],rest))


