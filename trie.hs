import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
            deriving Show

type Word = String

empty :: Trie
empty = (Trie {end = False, children = M.empty})

insert :: Word -> Trie -> Trie
insert [] (Trie e c) =  (Trie True c)
insert wrd@(w:ws) (Trie e c)
    | M.lookup w c == Nothing                                 = Trie e (M.insert w (insert ws empty) c)
    | otherwise                                               = Trie e (M.insert w (insert ws (fromJust (M.lookup w c))) c)