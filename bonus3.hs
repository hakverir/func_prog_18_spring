-- Pelin Hakverir
-- 150140031

-- word -> string, does not contain punctuation, whitespace etc
-- sentence -> list of words
-- character count -> mapping from chars to ints. list of (Char, Int) or Map
-- examine fromList and fromListWith funcs

type AWord = String

type Sentence = [String]

type CharacterCount = [(Char, Int)]

wordCharCounts :: AWord -> CharacterCount
wordCharCounts [] = []
wordCharCounts wrd@(x:xs) = (x, (length(filter (\y -> y == x) xs) + 1)) : wordCharCounts (dropChars wrd x)
    where
        dropChars :: AWord -> Char -> AWord
        dropChars ys ch
            | ys            == []   = []
            | head ys       == ch   = dropChars (tail ys) ch
            | otherwise             = head ys : dropChars (tail ys) ch




-- sentenceCharCounts :: [a] -> CharacterCount

-- dictCharCounts :: [Word] -> [(String, [(Char, Int)])]

-- dictWordsByCharCounts :: [(String, [(Char, Int)])] -> [([(Char, Int)], [String])] 

-- wordAnagrams :: Word -> [([(Char, Int)], [String])] -> [String]

-- charCountsSubsets :: CharacterCount -> [(Char, Int)]