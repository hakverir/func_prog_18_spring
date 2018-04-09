data Color = Red | Black
             deriving (Eq, Show)

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Eq, Show)

data Rank = Num Int | Jack | Queen | King | Ace
            deriving (Eq, Show)

data Card = Card { suit :: Suit, rank :: Rank }
            deriving (Eq, Show)

data Move = Draw | Discard Card
            deriving (Eq, Show)

cardColor :: Card -> Color
cardColor (Card {suit = c, rank = x}) = case c of
    Clubs    -> Black
    Spades   -> Black
    Diamonds -> Red
    Hearts   -> Red

cardValue :: Card -> Int
cardValue (Card {suit = c, rank = d}) = case d of
    Ace   -> 11
    Num x -> x
    _     -> 10

removeCard :: [Card] -> Card -> [Card]
removeCard cs c
    | null cs            = error "The card doesn't exist in the held cards"
    | (head cs) == c     = tail cs
    | otherwise          = (head cs) : removeCard (tail cs) c

allSameColor :: [Card] -> Bool
allSameColor []  = True
allSameColor [x] = True
allSameColor cs@(x1:x2:xs)
    | cardColor x1 == cardColor x2   = allSameColor (x2:xs)
    | otherwise                      = False 

sumCards :: [Card] -> Int
sumCards cs = sumHelper cs 0
    where
        sumHelper :: [Card] -> Int -> Int
        sumHelper [] a     = a
        sumHelper (x:xs) a = sumHelper xs (a + cardValue x)

preliminaryScore :: Int -> Int -> Int
preliminaryScore sum_held goal = if sum_held == 0 then 0 else if sum_held > goal then 3 * (sum_held - goal) else goal - sum_held

-- held cards and goal, returns score
score :: [Card] -> Int -> Int
score cs i
    | (allSameColor cs) == True  = (preliminaryScore (sumCards cs) i) `div` 2 
    | otherwise                  = preliminaryScore (sumCards cs) i

data State = DrawFromCards | CheckScore | TakeMove | DiscardFromHeld | EndOfGame
             deriving (Eq, Show)

-- card list, move list and goal, returns score
runGame :: [Card] -> [Move] -> Int -> Int
runGame cs ms goal = runHelper TakeMove cs [] ms goal
    where
        runHelper :: State -> [Card] -> [Card] -> [Move] -> Int -> Int
        runHelper st us hs zs gl --state, cards, held cards, moves, goal
            |  st == EndOfGame                       = score hs gl
            | (st == DrawFromCards) && (null us)     = runHelper EndOfGame us hs (tail zs) gl
            | (st == DrawFromCards) && (us /= [])    = runHelper CheckScore (tail us) ((head us) : hs) (tail zs) gl
            |  st == CheckScore                      = if ((sumCards hs) > gl) then runHelper EndOfGame us hs zs gl
                                                                               else runHelper TakeMove us hs zs gl
            | (st == TakeMove)      && (zs /= [])    = if ((head zs) == Draw) then runHelper DrawFromCards us hs zs gl
                                                                      else runHelper DiscardFromHeld us hs zs gl
            | (st == TakeMove)      && (null zs)     = runHelper EndOfGame us hs zs gl
            |  st == DiscardFromHeld                 = runHelper TakeMove us (removeCard hs (pickCard (head zs))) (tail zs) gl

        pickCard :: Move -> Card
        pickCard (Discard x) = x

convertSuit :: Char -> Suit
convertSuit x = case x of
    'c' -> Clubs
    'C' -> Clubs
    'd' -> Diamonds
    'D' -> Diamonds
    'h' -> Hearts
    'H' -> Hearts
    's' -> Spades
    'S' -> Spades
    _   -> error "There is no such kind of a suit"

convertRank :: Char -> Rank
convertRank r = case r of
    '1'  -> Ace
    '2'  -> Num 2
    '3'  -> Num 3
    '4'  -> Num 4
    '5'  -> Num 5
    '6'  -> Num 6
    '7'  -> Num 7
    '8'  -> Num 8
    '9'  -> Num 9
    't'  -> Num 10
    'T'  -> Num 10
    'j'  -> Jack
    'J'  -> Jack
    'q'  -> Queen
    'Q'  -> Queen
    'k'  -> King
    'K'  -> King
    _    -> error "There is no such kind of a rank"

convertCard :: Char -> Char -> Card
convertCard a b = (Card {suit = convertSuit a, rank = convertRank b})

readCards :: IO [Card]
readCards = do m <- getChar
               if      m == '\n'
                then do x <- readCards
                        return x
               else if m /= '.' 
                then 
                    do  n <- getChar
                        let y = convertCard m n
                        do x <- readCards
                           return (y : x)
               else
                    do temp <- getChar
                       return []

convertMove :: Char -> Char -> Char -> Move
convertMove k l m = case k of
    'd' -> Draw
    'D' -> Draw
    'r' -> Discard (convertCard l m)
    'R' -> Discard (convertCard l m)
    _   -> error "There is no such kind of a move"

readMoves :: IO [Move]
readMoves = do m <- getChar
               if      m == '\n'
                    then do x <- readMoves
                            return x
               else if m == '.' 
                    then do temp <- getChar 
                            return []
               else if m == 'r' || m == 'R'  
                    then 
                        do  n <- getChar
                            l <- getChar
                            let y = convertMove m n l
                            do x <- readMoves
                               return (y : x)
               else
                    do let y = convertMove m 'a' 'b'
                       do x <- readMoves
                          return (y : x)
                    

main = do putStrLn "Enter cards:"
          cards <- readCards
          -- putStrLn (show cards)

          putStrLn "Enter moves:"
          moves <- readMoves
          -- putStrLn (show moves)

          putStrLn "Enter goal:"
          line <- getLine

          let goal = read line :: Int

          let score = runGame cards moves goal
          putStrLn ("Score: " ++ show score)
