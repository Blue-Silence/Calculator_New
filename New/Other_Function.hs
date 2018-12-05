module Other_Function(
    headStringIf --判断输入的第一个String是否为第二个String头部
   ,headStringListIf --判断输入的第一个String是否为第二个String List中某个String的头部
   ,getStringPairInString --从第一个String头部取出存在于第二个String List的最长String，与剩余部分组成二元组返回
   ,add --向ItemList List中添加项(插在小于插入项的项前面或平级项内)
   ,get --从二元组列表中取出输入项所对应的项组成的表，若没有则返回[]
)where
import DataType

headStringIf :: String->String->Bool
headStringIf [] _ =True
headStringIf _ [] =False
headStringIf (x:xs) (y:ys)=and [(x==y),(headStringIf xs ys)]

headStringListIf :: String->[String]->Bool
headStringListIf [] _ =True
headStringListIf _ [] =False
headStringListIf x (y:ys)=or [(headStringIf x y),(headStringListIf x ys)]

getStringPairInString :: String->[String]->(String,String)
getStringPairInString=getStringPairInString_a []

getStringPairInString_a :: String->String->[String]->(String,String)
getStringPairInString_a former [] _=(former,[])
getStringPairInString_a former (x:xs) list
    |headStringListIf (former++[x]) list=getStringPairInString_a (former++[x]) xs list
    |otherwise=(former,(x:xs))

add :: (Ord a)=>[ItemList a b c]->(a,(b,c))->[ItemList a b c]
add [] (a,(b,c))=[ItemList a [(b,c)]]
add (ori@(ItemList a [(b,c)]):xs) (x,(y,z))
    |a==x=(ItemList a ((y,z):[(b,c)])):xs
    |a>x=ori:(add xs (x,(y,z)))
    |a<x=(ItemList a [(b,c)]):ori:xs

get :: (Eq a)=>[(a,b)]->a->[b]
get [] _=[]
get ((objname,obj):rest) name
    |objname==name=[obj]
    |otherwise=get rest name

