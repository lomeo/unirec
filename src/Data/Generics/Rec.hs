{- |
This module works with recursive data structure with cycles.
-}
module Data.Generics.Rec (
    universeRec
)
where

import Data.Generics
import Control.Monad.State
import Data.Set
import System.Mem.StableName
import System.IO.Unsafe

type RecState a = StateT (Set Int) IO a

goRec :: (Data v, Data r) => v -> RecState [r]
goRec !v = do
    hash <- hashStableName `fmap` liftIO (makeStableName v)
    p <- gets (member hash)
    if p
        then return []
        else do
            modify $ insert hash
            case cast v of
                Just v0 -> (v0:) `fmap` continue
                Nothing -> continue
    where
        continue = concat `fmap` sequence (gmapQ goRec v)

-- | Get all distinct children of a node, including itself and all children.
--
-- >   dataRose = Rose { roseId :: Int, roses :: [Rose] }
-- >       deriving (Data,Typeable)
--
-- >   roses =
-- >       let a = Rose 1 [b,c]
-- >           b = Rose 2 [a,c]
-- >           c = Rose 3 [a,b]
-- >       in a
--
-- > [ i | Rose i _ <- universeRec roses ]
-- > [1,2,3]
universeRec :: (Data v, Data r) => v -> [r]
universeRec v = unsafePerformIO $ evalStateT (goRec v) empty

