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


dictCharCounts :: [AWord] -> [(String, [(Char, Int)])]
dictCharCounts [] = []
dictCharCounts xs = (head xs, wordCharCounts (head xs)) : dictCharCounts (tail xs)


dictWordsByCharCounts :: [(String, [(Char, Int)])] -> [([String], [(Char, Int)])]
dictWordsByCharCounts [] = []
dictWordsByCharCounts xs@(x':xs') = (mergeWords x' xs') : dictWordsByCharCounts (dropFound xs)
    where
            mergeWords :: (String, [(Char, Int)]) -> [(String, [(Char, Int)])] -> ([String], [(Char, Int)])
            mergeWords (a, as) [] = ([a], as)
            mergeWords (a, as) bs = ( (a : map fst (filter (\j -> checkLetters as (snd j) ) bs) ) , as)

            dropFound :: [(String, [(Char, Int)])] -> [(String, [(Char, Int)])]
            dropFound [] = []
            dropFound ts = filter (\b -> (checkLetters (snd $ head ts) (snd b)) == False ) ts

            checkLetters :: [(Char, Int)] -> [(Char, Int)] -> Bool
            checkLetters [] [] = True
            checkLetters ys zs 
                | length(filter (\p -> (head ys) == p) zs) == 0    = False
                | otherwise                                        = checkLetters (tail ys) (filter (\r -> head ys /= r) zs)

-- wordAnagrams :: Word -> [([(Char, Int)], [String])] -> [String]

-- charCountsSubsets :: CharacterCount -> [(Char, Int)]