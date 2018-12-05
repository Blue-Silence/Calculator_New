module DataType
(   
    Exp(..)
   ,ItemList(..)
   ,Number
)where
data Exp = Opt [Char] | Exp [Exp] | Num Double | Complex [Exp]
    deriving (Show,Eq)

type Number=Double

data ItemList a b c=ItemList a [(b,c)]

