module Main where

import Test.QuickCheck
import Data.Data
import Data.Generics.Rec

data Rose = Rose { roseId :: Int, roseBranches :: [Rose] }
    deriving (Data,Typeable)

roses =
    let a = Rose 1 [b,c]
        b = Rose 2 [a,c]
        c = Rose 3 [a,b]
    in a

data A = A B
    deriving (Data,Typeable)
data B = B A
    deriving (Data,Typeable)

recLimit_prop = length [ r | r@Rose{} <- universeRec roses ] == 3
recOrder_prop = [ i | Rose i _ <- universeRec roses] == [1,2,3]

recAB_prop = length [ b | b@B{} <- universeRec a ] == 1
    where
        a = A b
        b = B a

main = do
    quickCheck recLimit_prop
    quickCheck recOrder_prop
    quickCheck recAB_prop
    return ()
