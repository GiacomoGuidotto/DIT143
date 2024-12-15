module Lab5 where

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

readQAFromFile :: FilePath -> IO QA
readQAFromFile fname = do
  res <- tryIOError (readFile fname)
  case res of
    Left _ -> return defaultTree
    Right contents ->
      case readEither contents of
        Left _ -> return defaultTree
        Right qa -> return qa

isYes :: String -> Bool
isYes ans = case map toLower ans of
  ('y' : _) -> True
  _ -> False

askQuestions :: QA -> IO QA
askQuestions (Person name) = do
  putStr ("My guess: Is it " ++ name ++ "? ")
  hFlush stdout
  ans <- getLine
  if isYes ans
    then do
      putStrLn "I won!"
      return (Person name)
    else do
      putStrLn "OK - you won this time."
      learnNew (Person name)
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

learnNew :: QA -> IO QA
learnNew (Person oldName) = do
  putStr "Just curious: Who was your famous person? "
  hFlush stdout
  newPerson <- getLine
  putStrLn $ "Give me a question for which the answer for \"" ++ newPerson ++ "\" is \"yes\""
  putStrLn $ "and the answer for \"" ++ oldName ++ "\" is \"no\"."
  hFlush stdout
  newQ <- getLine
  let newNode = Question newQ (Person newPerson) (Person oldName)
  return newNode
learnNew _ = error "learnNew called on a Question node, which should not happen."

play :: QA -> IO QA
play tree = do
  newTree <- askQuestions tree
  putStr "Play again? (yes/no): "
  hFlush stdout
  ans <- getLine
  if isYes ans
    then play newTree
    else return newTree

main :: IO ()
main = do
  tree <- readQAFromFile "questions.qa"
  finalTree <- play tree
  writeFile "questions.qa" (show finalTree)
