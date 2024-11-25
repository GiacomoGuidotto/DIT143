-- module Sudoku
module Lab3.Sudoku
  ( Sudoku (..),
    rows,
    allBlankSudoku,
    example,
    isSudoku,
    isFilled,
    readSudoku,
    printSudoku,
    cell,
    prop_Sudoku,
    isOkayBlock,
    blocks,
    prop_blocks_lengths,
    isOkay,
    blanks,
    prop_blanks_allBlanks,
    (!!=),
    prop_bangBangEquals_correct,
    update,
    prop_update_updated,
  )
where

import Data.Char (digitToInt)
import Data.List (nub, transpose)
import Data.Maybe (catMaybes, isNothing)
import Test.QuickCheck

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell

type Row = [Cell] -- a row is a list of cells

newtype Sudoku = Sudoku [Row]
  deriving (Show, Eq)

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

cols :: Sudoku -> [Row]
cols = transpose . rows

-- | A sample sudoku puzzle
example :: Sudoku
example =
  Sudoku
    [ [j 3, j 6, n, n, j 7, j 1, j 2, n, n],
      [n, j 5, n, n, n, n, j 1, j 8, n],
      [n, n, j 9, j 2, n, j 4, j 7, n, n],
      [n, n, n, n, j 1, j 3, n, j 2, j 8],
      [j 4, n, n, j 5, n, j 2, n, n, j 9],
      [j 2, j 7, n, j 4, j 6, n, n, n, n],
      [n, n, j 5, j 3, n, j 8, j 9, n, n],
      [n, j 8, j 3, n, n, n, n, j 6, n],
      [n, n, j 7, j 6, j 9, n, n, j 4, j 3]
    ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = hasValidDimensions && allCellsValid
  where
    hasValidDimensions =
      length (rows s) == 9
        && all ((== 9) . length) (rows s)
    allCellsValid = all (all isCellValid) (rows s)

    isCellValid Nothing = True
    isCellValid (Just n) = n >= 1 && n <= 9

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled = all (all isJust) . rows
  where
    isJust Nothing = False
    isJust _ = True

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStrLn $ unlines $ map (concatMap showCell) (rows s)
  where
    showCell :: Cell -> String
    showCell Nothing = "."
    showCell (Just n) = show n

-- * B2

-- | parseSudoku takes a string representing a sudoku puzzle and returns
-- Just the puzzle if it is valid, and Nothing otherwise
parseSudoku :: String -> Maybe Sudoku
parseSudoku text = do
  rs <- parseLines (lines text)
  return (Sudoku rs)
  where
    parseLines :: [String] -> Maybe [Row]
    parseLines ls
      | length ls /= 9 = Nothing
      | otherwise = mapM parseLine ls

    parseLine :: String -> Maybe Row
    parseLine l
      | length l /= 9 = Nothing
      | otherwise = mapM parseCell l

    parseCell :: Char -> Maybe Cell
    parseCell '.' = Just Nothing
    parseCell c
      | c >= '1' && c <= '9' = Just $ Just $ digitToInt c
      | otherwise = Nothing

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku p = do
  content <- readFile p
  case parseSudoku content of
    Just s -> return s
    Nothing -> error "Invalid sudoku"

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [(9, return Nothing), (1, Just <$> choose (1, 9))]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = Sudoku <$> vectorOf 9 (vectorOf 9 cell)

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell

-- * D1

-- | hasUniqueJusts checks if a list of Maybe values is unique ignoring Nothings
hasUniqueJusts :: (Eq a) => [Maybe a] -> Bool
hasUniqueJusts xs =
  let justValues = catMaybes xs
   in length justValues == length (nub justValues)

-- | isOkayBlock checks if a block is valid following the sudoku rules
isOkayBlock :: Block -> Bool
isOkayBlock b = length b == 9 && hasUniqueJusts b

-- * D2

-- | chunkOf takes a list and returns a list of lists of n elements each
chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs = take n xs : chunkOf n (drop n xs)

-- | blocks takes a sudoku and returns a list of all blocks
blocks :: Sudoku -> [Block]
blocks s = rows s ++ cols s ++ getBlocks s

-- | getBlocks takes a sudoku and returns a list of all blocks
getBlocks :: Sudoku -> [Block]
getBlocks s =
  let -- groups of 3 rows each
      rowsGroups = chunkOf 3 (rows s)

      -- get the three blocks from the three rows
      blocksInGroup rowsGroup =
        let -- split all 3 rows in 3 chunks each
            chunkedRowsGroup = map (chunkOf 3) rowsGroup
            -- transpose the 9 chunk in the group to
            -- move the three chunk in the same column in one row
            chunkedBlocksGroup = transpose chunkedRowsGroup
         in -- concat each chunk in the row
            map concat chunkedBlocksGroup
   in -- get the blocks for each groups of rows and concat them
      concatMap blocksInGroup rowsGroups

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s = length bs == 9 * 3 && all ((== 9) . length) bs
  where
    bs = blocks s

-- * D3

isOkay :: Sudoku -> Bool
isOkay s =
  all isOkayBlock (blocks s)
    && all hasUniqueJusts (rows s)
    && all hasUniqueJusts (cols s)

---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------

-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int, Int)

-- * E1

-- | blanks takes a sudoku and returns a list of blank positions
blanks :: Sudoku -> [Pos]
blanks s =
  [(i, j) | (i, r) <- zip [0 ..] (rows s), (j, v) <- zip [0 ..] r, isNothing v]

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks =
  let expected = [(i, j) | i <- [0 .. 8], j <- [0 .. 8]]
   in blanks allBlankSudoku == expected

-- * E2

-- | (!!=) replaces the ith element of a list
(!!=) :: [a] -> (Int, a) -> [a]
xs !!= (i, y)
  | i >= 0 && i < length xs = take i xs ++ [y] ++ drop (i + 1) xs
  | otherwise = xs

prop_bangBangEquals_correct :: [Int] -> (Int, Int) -> Bool
prop_bangBangEquals_correct xs (i, x)
  | i >= 0 && i < length xs = (xs !!= (i, x)) !! i == x
  | otherwise = xs !!= (i, x) == xs

-- * E3

-- | update sud pos cell returns a sudoku where the cell at pos is set to cell
update :: Sudoku -> Pos -> Cell -> Sudoku
update s (i, j) v
  | i `elem` [0 .. 8] && j `elem` [0 .. 8] = Sudoku $ rows s !!= (i, (rows s !! i) !!= (j, v))
  | otherwise = s

prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated s (i, j) v
  | i `elem` [0 .. 8] && j `elem` [0 .. 8] = (rows (update s (i, j) v) !! i !! j) == v
  | otherwise = update s (i, j) v == s

------------------------------------------------------------------------------

-- * F1

-- * F2

-- * F3

-- * F4
