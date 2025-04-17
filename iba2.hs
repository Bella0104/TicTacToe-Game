module IBA2 where

import Base
import DecisionTree

-- Game mode: against another player or AI
data GameMode = VsAI | VsPlayer deriving (Eq)


-- Empty board
emptyBoard :: Board
emptyBoard = replicate 9 Nothing

-- Check win condition
hasWon :: Player -> Board -> Bool
hasWon player board =
  any (\line -> all (== Just player) line) winningLines
  where
    winningLines = [[board !! i | i <- line] | line <- winningIndices]
    winningIndices =
      [[0,1,2], [3,4,5], [6,7,8], -- Rows
       [0,3,6], [1,4,7], [2,5,8], -- Columns
       [0,4,8], [2,4,6]]          -- Diagonals

-- Board full
isFull :: Board -> Bool
isFull = all (/= Nothing)

-- Make a move
makeMove :: Player -> Int -> Board -> Maybe Board
makeMove player pos board
  | pos < 0 || pos >= length board = Nothing
  | board !! pos /= Nothing = Nothing
  | otherwise = Just (take pos board ++ [Just player] ++ drop (pos + 1) board)

-- Show the board
printBoard :: Board -> IO ()
printBoard board = putStrLn $ unlines $ formatBoard board
  where
    formatBoard b = [row [b !! i, b !! (i+1), b !! (i+2)] | i <- [0,3,6]]
    row [a, b, c] = unwords [showCell a, showCell b, showCell c]
    showCell Nothing    = "_"
    showCell (Just One) = "X"
    showCell (Just Two) = "O"

-- Convert board to row of strings for AI
boardToRow :: Board -> Row
boardToRow = map toVal
  where
    toVal Nothing = "-"
    toVal (Just One) = "X"
    toVal (Just Two) = "O"

-- Hardcoded example header and tree (very simple)
exampleHeader :: Header
exampleHeader = ["0", "1", "2", "3", "4", "5", "6", "7", "8"]

-- Dummy decision tree: always pick the first empty space
aiDecisionTree :: DecisionTree
aiDecisionTree = Node "0"
  [ ("-", Leaf (Yes, 1.0))
  , ("X", Node "1"
      [ ("-", Leaf (Yes, 1.0))
      , ("X", Node "2"
          [ ("-", Leaf (Yes, 1.0))
          , ("X", Leaf (No, 1.0))
          , ("O", Leaf (No, 1.0))
          ])
      , ("O", Leaf (No, 1.0))
      ])
  , ("O", Leaf (No, 1.0))
  ]

-- AI Move using decision tree: picks the first 'Yes' label
aiMove :: Board -> Int
aiMove board = case nextMove (infer aiDecisionTree exampleHeader (boardToRow board)) of
    Just idx -> idx
    Nothing -> head [i | (i, v) <- zip [0..] board, v == Nothing]
  where
    nextMove (Just (Yes, _)) = Just (head [i | (i, v) <- zip [0..] board, v == Nothing])
    nextMove _               = Nothing

-- Main game loop
gameLoop :: GameMode -> Player -> Board -> IO ()
gameLoop mode player board
  | hasWon One board = putStrLn "Player One wins!"
  | hasWon Two board = putStrLn (if mode == VsAI then "AI wins!" else "Player Two wins!")
  | isFull board = putStrLn "It's a draw!"
  | otherwise = do
      pos <- case (mode, player) of
        (_, One) -> do
          putStrLn "Player One, enter a position (0-8):"
          readLn
        (VsPlayer, Two) -> do
          putStrLn "Player Two, enter a position (0-8):"
          readLn
        (VsAI, Two) -> do
          putStrLn "AI is making a move..."
          return $ aiMove board

      case makeMove player pos board of
        Just newBoard -> do
          printBoard newBoard
          gameLoop mode (flipPlayer player) newBoard
        Nothing -> do
          putStrLn "Invalid move! Try again."
          gameLoop mode player board

-- Entry point
startGame :: IO ()
startGame = do
  putStrLn "Welcome to Tic-Tac-Toe!"
  putStrLn "Choose mode: (1) Player vs Player, (2) Player vs AI)"
  choice <- getLine
  let mode = if choice == "1" then VsPlayer else VsAI
  printBoard emptyBoard
  gameLoop mode One emptyBoard
