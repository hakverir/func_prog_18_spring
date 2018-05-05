-- Pelin Hakverir
-- 150140031

-- word -> string, does not contain punctuation, whitespace etc
-- sentence -> list of words
-- character count -> mapping from chars to ints. list of (Char, Int) or Map
-- examine fromList and fromListWith funcs

import Data.Char

type AWord = String

type Sentence = [String]

type CharacterCount = [(Char, Int)]

wordCharCounts :: AWord -> CharacterCount
wordCharCounts [] = []
wordCharCounts wrd@(x:xs) = (x, (length(filter (\y -> (toLower y) == (toLower x)) xs) + 1)) : wordCharCounts (dropChars wrd x)
    where
        dropChars :: AWord -> Char -> AWord
        dropChars ys ch
            | ys                        == []             = []
            | (toLower (head ys))       == (toLower ch)   = dropChars (tail ys) ch
            | otherwise                                   = head ys : dropChars (tail ys) ch


sentenceCharCounts :: String -> CharacterCount
sentenceCharCounts [] = []
sentenceCharCounts xs = wordCharCounts $ dropSpaces xs
    where
        dropSpaces :: String -> String
        dropSpaces ys
            | ys              == []     = []
            | head ys         == ' '    = dropSpaces (tail ys)
            | otherwise                 = head ys : dropSpaces (tail ys)

-- dictCharCounts :: [Word] -> [(String, [(Char, Int)])]

-- dictWordsByCharCounts :: [(String, [(Char, Int)])] -> [([(Char, Int)], [String])] 

-- wordAnagrams :: Word -> [([(Char, Int)], [String])] -> [String]

-- charCountsSubsets :: CharacterCount -> [(Char, Int)]