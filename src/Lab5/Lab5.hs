module Lab5
  ( QA (Person, Question),
    defaultTree,
    readQAFromFile,
    isYes,
    askQuestions,
    learnNew,
    play,
    main,
  )
where

import Data.Char (toLower)
import System.IO
import System.IO.Error (tryIOError)
import Text.Read (readEither)

data QA
  = Person String
  | Question String QA QA
  deriving (Show, Read)

defaultTree :: QA
defaultTree =
  Question
    "Is this person from Europe?"
    ( Question
        "Is this person a scientist?"
        (Person "Marie Curie")
        (Person "Maggie Thatcher")
    )
    ( Question
        "Is this person an actor?"
        (Person "Marilyn Monroe")
        (Person "Hillary Clinton")
    )

-- initialize the QA tree by reading from a file and parsing the content
-- fallback to the default tree if the file is not found or the content is
-- not parsable
readQAFromFile :: FilePath -> IO QA
readQAFromFile fname = do
  res <- tryIOError (readFile fname)
  case res of
    Left _ -> return defaultTree
    Right contents ->
      case readEither contents of
        Left _ -> do
          putStrLn "Using the default quiz data."
          return defaultTree
        Right qa -> return qa

-- utility function to check if the user's input starts with 'y'
-- to determine if the answer is yes (case-insensitive)
isYes :: String -> Bool
isYes ans = case map toLower ans of
  ('y' : _) -> True
  _ -> False

-- singe match loop
-- traverse the tree top-down to ask questions, rebuild the tree bottom-up
-- to change the leaf node in case the computer's guess is wrong
askQuestions :: QA -> IO QA
-- leaf node: computer's guess
askQuestions (Person name) = do
  putStr ("My guess: Is it " ++ name ++ "? ")
  hFlush stdout
  ans <- getLine
  if isYes ans
    then do
      putStrLn "Hurray! I won!"
      return (Person name)
    else do
      putStrLn "OK - you won this time."
      learnNew (Person name)
-- internal node: ask a question
askQuestions (Question q yesTree noTree) = do
  putStr (q ++ " ")
  hFlush stdout
  ans <- getLine
  if isYes ans
    then do
      newYes <- askQuestions yesTree
      return (Question q newYes noTree)
    else do
      newNo <- askQuestions noTree
      return (Question q yesTree newNo)

-- build a new node by asking the user the question
learnNew :: QA -> IO QA
learnNew (Person oldName) = do
  putStr "Just curious: Who was your famous person? "
  hFlush stdout
  newPerson <- getLine
  putStrLn $ "Give me a question for which the answer for \"" ++ newPerson ++ "\" is \"yes\""
  putStrLn $ "and the answer for \"" ++ oldName ++ "\" is \"no\"."
  hFlush stdout
  newQ <- getLine
  return (Question newQ (Person newPerson) (Person oldName))
learnNew _ = error "learnNew called on a Question node, which should not happen."

-- game loop
-- generate a new tree after each match, uses it to play again or returns it
play :: QA -> IO QA
play tree = do
  newTree <- askQuestions tree
  putStr "Play again? "
  hFlush stdout
  ans <- getLine
  if isYes ans
    then play newTree
    else return newTree

-- parse tree from file, mutate it during the game, and write it back to the
-- file
main :: IO ()
main = do
  tree <- readQAFromFile "questions.qa"
  finalTree <- play tree
  putStrLn "Saving QA file..."
  writeFile "questions.qa" (show finalTree)
  putStrLn "Bye!"
