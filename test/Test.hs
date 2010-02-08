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

recLimit_prop = length [ r | r@Rose{} <- universeRec roses ] == 3
recOrder_prop = [ i | Rose i _ <- universeRec roses] == [1,2,3]

main = do
    quickCheck recLimit_prop
    quickCheck recOrder_prop
    return ()
