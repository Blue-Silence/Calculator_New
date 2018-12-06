module ValueMain(valueMain)where

import Transfer
import Value
import DataType
import Tools

type Opttype=[Exp]->[Exp]->(Exp,[Exp],[Exp])

valueMain :: [ItemList Integer String Opttype]->String->Exp
valueMain optlist=valueall list . transfer listName
                        where list=map (clothesOff) optlist
                              listName=foldl1 (++) (map (map ccar) list)

