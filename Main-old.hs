module Main where

import System.Environment (getArgs)
{-# LANGUAGE TypeSynonymInstances #-}
import Data.Char (isAlpha)
import qualified Data.Map as M (Map, empty, insert, foldlWithKey, fromList)
import Data.List(isPrefixOf)

data TaskType = Simple | Ordered | Unordered deriving Show

data PropType = Description | DueDate | Priority | Points deriving (Show, Eq, Ord)

type Body = M.Map PropType String

data Task = SimpleTask Body
          | OrderedTasks [Task]
          | UnorderedTasks [Task]
          deriving Show

--instance show Task where
--  show aTask = showSubTaskWithOffset aTask 0
--    where showSubTaskWithOffset (SimpleTask body) offset =  map (nspaces offset ++) bodyLines body
--          nspaces n = replicate n ' '
  

main :: IO ()
main = do
  [fileName] <- getArgs
  txt <- fmap lines $ readFile fileName
  putStrLn (show $ parseFile txt)


parseFile :: [String] -> Task
parseFile file = case tType of
  Simple    -> SimpleTask (getBody file)
  Ordered   -> OrderedTasks (map parseFile (getListItems '-' file))
  Unordered -> UnorderedTasks (map parseFile (getListItems '+' file))
  where tType = taskType (head $ head file)


getListItems :: Char -> [String] -> [[String]]
getListItems _ [] = []
getListItems char txt = [item] ++ (getListItems char rest)
  where (firstMinusHead, rest) = break ((== char) . head) (tail txt)
        item = map (drop 2) ([head txt] ++ firstMinusHead)
       
getBody :: [String] -> Body
getBody [] = M.empty
getBody body = getProperties $ getDescription
  where getDescription :: ([String], M.Map PropType String)
        getDescription 
          |description == [] = (body, M.empty)
          |otherwise         = (remainder, M.insert Description (init $ formatLines description) M.empty) 
          where formatLines :: [String] -> String
                formatLines desc = if length desc == 1 then (init $ unlines description)
                                     else unlines $ [head desc] ++ (map (replicate 14 ' ' ++) (tail desc))
        (description,remainder) = break hasProperty body
        hasProperty line
          |"due" `isPrefixOf` line        = True
          |"priority: " `isPrefixOf` line = True
          |"points: " `isPrefixOf` line   = True
          |otherwise                      = False 
        getProperties :: ([String], M.Map PropType String) -> M.Map PropType String
        getProperties ([], mapIn) = mapIn
        getProperties (line : moreLines, mapIn)
          |"due: " `isPrefixOf`line = getProperties (moreLines, M.insert DueDate (drop 5 line) mapIn) 
          |"priority: " `isPrefixOf`line = getProperties (moreLines, M.insert Priority (drop 10 line) mapIn)
          |"points: " `isPrefixOf`line = getProperties (moreLines, M.insert Points (drop 8 line) mapIn)
  

taskType :: Char -> TaskType
taskType c
  |c == '-'  = Ordered
  |c == '+'  = Unordered
  |isAlpha c = Simple
  |otherwise = error "\n\n\nERROR:  Line must begin with '-', '+', or a letter"


showBody :: Body -> String
showBody = M.foldlWithKey (\str k v -> str ++ "\n\n" ++ show k ++ ":" ++ replicate (13 - (length (show k))) ' ' ++ v) ""

bodyLines :: Body -> [String]
bodyLines = M.foldlWithKey (\blines k v -> blines ++ ["\n","\n"] ++ [show k ++ ":" ++ replicate (13 - (length (show k))) ' ' ++ v]) []


testbody :: String
testbody = "This is just a test body\nwith two lines of description\ndue: 9/18/52\npriority: Top\npoints: 1"

showSubTaskWithOffset (SimpleTask body) offset =  map (nspaces offset ++) (bodyLines body)
  where nspaces n = replicate n ' '  


-- Test Data
testBody :: Body
testBody = M.fromList [(Description,"This is just a test body\nwith two lines of description"), (DueDate, "9/18/52"), (Priority, "Top"), (Points, "1")] 

taskList :: [String]
taskList = lines "- This is the first in a list\n  Some description of first\n- This is the second in a list\n  Description of second\n  due 7/1/18"


