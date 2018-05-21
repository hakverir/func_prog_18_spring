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

insert :: Word -> Trie -> Trie --While you are adding words, sometimes you need to create a new node, you can use the empty function for this purpose.
insert [] t = t
insert wrd@(w:ws) (Trie {end = e, children = c})
    | ws           == []      &&    M.lookup w c == Nothing   = (Trie {end = e, children = (M.insert w (Trie {end = True, children = M.empty}) c) })
    -- | ws           == []                                      = (Trie {end = e, children = M.insert w (Trie {end = True, children = c}) c})
    | M.lookup w c == Nothing                                 = (Trie {end = e, children = M.insert w (insert ws (Trie {end = e, children = c})) c})       --harf eklenecek, kelime ayni sekilde gonderilecek ve ilk harfinin triede oldugunu gorup alt dalinda kelimenin kalan kismiyla devam edecek
    | otherwise                                               = (Trie {end = e, children = M.insert w (insert ws (returnTrie (M.lookup w c))) c})
    where
        returnTrie :: Maybe Trie -> Trie
        returnTrie (Just z) = z