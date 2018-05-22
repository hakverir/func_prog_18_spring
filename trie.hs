import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
            deriving (Eq, Show)

data Action = Add | Search | Find | Print | Exit
              deriving (Eq, Show)

type Word = String

type MyMap = M.Map Char Trie

empty :: Trie
empty = (Trie {end = False, children = M.empty})

insert :: Word -> Trie -> Trie
insert [] (Trie e c) =  (Trie True c)
insert wrd@(w:ws) (Trie e c)
    | M.lookup w c == Nothing                                 = Trie e (M.insert w (insert ws empty) c)
    | otherwise                                               = Trie e (M.insert w (insert ws (fromJust (M.lookup w c))) c)

insertList :: [Word] -> Trie
insertList [] = empty
insertList wrd = foldr insert empty wrd

search :: Word -> Trie -> Bool
search [] (Trie e c) = case e of 
    True  -> True 
    False -> False
search wrd@(w:ws) (Trie e c)
    | M.lookup w c == Nothing                                 = False
    | otherwise                                               = search ws (fromJust (M.lookup w c))

getWords :: Trie -> [Word] -- use toList //////  write a function that traverses the trie (while keeping track of where it came from) and also keeps an accumulator which found words are inserted into
getWords (Trie x y) = findWords [] [] [] (M.toList y)
    where
        findWords :: [(Char, Trie)] -> Word -> [Word] -> [(Char, Trie)] -> [Word]
        -- findWords wrd acc (Trie x y) (Trie z t)
        --     | 
        findWords acc wrd lst trace
            | trace                                                            == []        = lst
            | snd (head trace)                                                 == empty     = findWords [] [] lst (tail trace)   -- head of tracei gittikce bosaltmak lazim
            | length(returnChildren (head trace))                               > 1         = findWords (tail (returnChildren (head trace)) ++ acc) wrd lst (returnChildren (head trace))
            | isEnd (snd (head trace)) == True  && returnChildren (head trace) == []        = findWords [] [] (lst ++ [ wrd ++ [returnFirstChar trace]] ) (acc ++ tail trace)
            | isEnd (snd (head trace)) == True  && returnChildren (head trace) /= []        = findWords (acc) [] (lst ++ [ wrd ++ [returnFirstChar trace]] ) (returnChildren (head trace))
            | otherwise                                                                     = findWords (acc) (wrd ++ [fst (head trace)]) lst (returnChildren (head trace))

        isEnd :: Trie -> Bool
        isEnd (Trie a b) = a

        returnFirstChar :: [(Char, Trie)] -> Char
        returnFirstChar xs = fst (head xs)

        -- returnCharList :: [(Char, Trie)] -> [Char]
        -- returnCharList [] = []
        -- returnCharList xs = map fst xs

        returnChildren :: (Char, Trie) -> [(Char, Trie)]
        returnChildren (_, (Trie c d)) = M.toList d



prefix :: String -> Trie -> [String] --  It returns a list of strings that start with the given prefix. If there is no word starting with that prefix, it should return Nothing. You may want to use the getWords function to simplify the implementation of this function.
prefix s t = []

convertAction :: String -> Action
convertAction x = case x of
    "a"-> Add
    "A" -> Add
    "s" -> Search
    "S" -> Search
    "f" -> Find
    "F" -> Find
    "p" -> Print
    "P" -> Print
    "e" -> Exit
    "E" -> Exit
    _   -> error "There is no such kind of an action"

printList :: [String] -> IO ()
printList []        = return ()
printList xs@(y:ys) = do putStrLn y
                         printList ys

getInput :: Trie -> IO ()
getInput t = do putStrLn "Enter the action: "
                ch <- getLine
                let act = convertAction ch
                if act == Add
                    then do putStrLn "Enter word/prefix:"
                            addWord <- getLine
                            let o = insert addWord t
                            putStrLn "New word is added!"
                            getInput o
                else if act == Search
                    then do putStrLn "Enter word/prefix:"
                            searchWord <- getLine
                            if search searchWord t == True
                                then do putStrLn "Exists in dictionary!"
                            else
                                do putStrLn "NOT exist!"
                            getInput t
                else if act == Find
                    then do putStrLn "Enter word/prefix:"
                            findWord <- getLine
                            let prfx = prefix findWord t
                            if prfx == []
                                then do putStrLn "No words found with that prefix!"
                            else
                                do putStrLn "Found words:"
                                   printList prfx
                            getInput t
                else if act == Print
                    then do putStrLn "List of words in dictionary:"
                            let wrds = getWords t
                            printList wrds
                            getInput t
                else
                    do putStrLn "Mischief Managed"

main = do   putStrLn "a) Add Word"
            putStrLn "s) Search Word"
            putStrLn "f) Find words with prefix"
            putStrLn "p) Print all words"
            putStrLn "e) Exit"
            getInput empty


