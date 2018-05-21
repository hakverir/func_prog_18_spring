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