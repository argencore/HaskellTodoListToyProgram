module Schedule where

import Data.Time
import System.IO
import Data.Time.Clock.POSIX
import Data.Time.Format
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (forever)
import Control.Monad
import Control.Monad.Trans

-- task name startTimeFromEpoch endTimeFromEpoch
data Task = ScheduleTask String (Integer) (Integer) deriving (Show,Eq)
-- task name start utc time end utc time
data UTCTask = ScheduleUTCTask String (UTCTime) (UTCTime) deriving (Show)
-- task name start local time end local time
data LocalTask = ScheduleLocalTask String (LocalTime)(LocalTime) deriving (Show)


--from haskell.org
date :: IO (Integer, Int, Int) -- :: (year, month, day)
date = getCurrentTime >>= return . toGregorian . utctDay

getTimeAsIntegerFromEpoc :: IO Integer
getTimeAsIntegerFromEpoc = (round `fmap` getPOSIXTime) :: IO Integer

taskDescription :: Task -> String
taskDescription (ScheduleTask description date time) = description

taskStartTime :: Task -> (Integer)
taskStartTime (ScheduleTask description date time) = date

taskEndTime :: Task -> (Integer)
taskEndTime (ScheduleTask description date time) = time

integerToUTC :: Integer -> UTCTime
integerToUTC time = posixSecondsToUTCTime (fromInteger time :: POSIXTime) 

-- create new list of tasks using utc time
convertTasksToUTCTasks :: [Task] -> [UTCTask]
convertTasksToUTCTasks [] = []
convertTasksToUTCTasks [ScheduleTask description start end] = [(ScheduleUTCTask description (integerToUTC start) (integerToUTC end))] 
convertTasksToUTCTasks ((ScheduleTask description start end): xs) = (ScheduleUTCTask description (integerToUTC start) (integerToUTC end)) : convertTasksToUTCTasks xs

-- main menu line with options
mainMenuLine = "Option Menu:\nPress 1 to add an Item to your list\nPress 2 to remove an Item from your list\nPress 3 to print the list to a txt file\nPress 4 to look at current list\nPress 5 to get current time in UTC\nPress 6 to exit\nOVERDUE LIST:"

-- tell if task is overdue
overdueTask :: Task -> IO Bool
overdueTask x = do 
  tIFE <- getTimeAsIntegerFromEpoc
  if   taskEndTime x < tIFE then return True else return False

-- add task to list
addTask :: Task -> [Task] -> [Task]
addTask a b = a:b

-- remove task from list
removeTask :: Task -> [Task] -> [Task]
removeTask a (x:xs) | a == x = removeTask a xs
                    | otherwise = x : removeTask a xs

taskOverlapCheck :: Task -> [Task] -> Bool
taskOverlapCheck task [] = False
taskOverlapCheck task (t:tasks) | (taskStartTime task >= taskStartTime t && taskStartTime task <= taskEndTime t) || (taskEndTime task >= taskStartTime t && taskEndTime task <= taskEndTime t) || (taskEndTime task >= taskEndTime t && taskStartTime task <= taskStartTime t) = True
                                | otherwise = taskOverlapCheck task tasks
-- check for overdue tasks and report them
checkTasks :: [Task] -> IO()
checkTasks tasks = do
  currentTime <- getTimeAsIntegerFromEpoc
  -- get tasks that have end times before current time
  let overdueTasks = filter(\t -> taskEndTime t <= currentTime) (tasks)
  -- print them out
  mapM_ print (convertTasksToUTCTasks overdueTasks)
  -- sleep for a while
  threadDelay (5000000)
  -- dont keep them around to repeate
  let currentList = filter (`notElem` overdueTasks) tasks
  checkTasks currentList


mainLoop :: [Task] -> IO()
mainLoop tasks = do
 putStrLn mainMenuLine
 -- create new thread for checking list constantly
 forkIO $ do
  checkTasks tasks
 -- create new channel for listening async
 input <- newChan:: IO (Chan String)
 -- create thread to handle input
 forkIO $ do
   userInput <- getLine
   writeChan input userInput
 -- handle output
 do
   userInput <- readChan input
   -- handle create new task
   if (userInput) == "1" then do
       putStrLn "please write the descrption of the task and then press enter"
       description <- getLine
       putStrLn "please enter the task start time exactly as follows day month year time AM/PM (ex. 26 Jan 2012 10:54 AM) and then press enter"
       startTime <- getLine
       putStrLn "please enter the end time exactly as follows day month year time AM/PM (ex. 26 Jan 2012 10:54 AM) and then press enter"
       endTime <- getLine
       -- get time zone
       timeZone <- getCurrentTimeZone
       -- parse times as local
       let sTime = readTime defaultTimeLocale "%d %b %Y %l:%M %p" startTime :: LocalTime
       let eTime = readTime defaultTimeLocale "%d %b %Y %l:%M %p" endTime :: LocalTime
       -- parse for task as Posix time
       let startPosix = (round (utcTimeToPOSIXSeconds (localTimeToUTC timeZone sTime))) :: Integer
       let endPosix = (round (utcTimeToPOSIXSeconds (localTimeToUTC timeZone eTime))) :: Integer
       -- create then add task
       let newTask = ScheduleTask (description)(startPosix)(endPosix)
       if taskOverlapCheck newTask tasks then do
        print "CANNOT CREATE EVENT: you have an event in that time slot already"
        mainLoop tasks
       else do
        let newTasks = newTask : tasks
        mainLoop newTasks
   -- handle task removal
   else if (userInput) == "2" then do
       putStrLn "please write the descrption of the task exactly and then press enter"
       description <- getLine
       putStrLn "please enter the task start time exactly as follows:\n day month year time AM/PM (ex. 26 Jan 2012 10:54 AM) and then press enter"
       startTime <- getLine
       putStrLn "please enter the end time exactly as follows:\n day month year time AM/PM (ex. 26 Jan 2012 10:54 AM) and then press enter"
       endTime <- getLine
       timeZone <- getCurrentTimeZone
       -- take the same steps as for creating a task
       let sTime = readTime defaultTimeLocale "%d %b %Y %l:%M %p" startTime :: LocalTime
       let eTime = readTime defaultTimeLocale "%d %b %Y %l:%M %p" endTime :: LocalTime
       let startPosix = (round (utcTimeToPOSIXSeconds (localTimeToUTC timeZone sTime))) :: Integer
       let endPosix = (round (utcTimeToPOSIXSeconds (localTimeToUTC timeZone eTime))) :: Integer
       let newTask = ScheduleTask (description)(startPosix)(endPosix)
       -- recurse on the list minus the task to remove
       mainLoop (removeTask newTask tasks)
   -- handle writing to a file
   else if (userInput) == "3" then do
    -- open or create file
    file <- openFile "schedule.txt" WriteMode
    -- convert to utc task
    let utc = convertTasksToUTCTasks tasks
    -- print one per line
    mapM_ (hPrint file) utc
    hClose file
    mainLoop tasks
  -- handle print to screen
   else if (userInput) == "4" then do
    let utc = convertTasksToUTCTasks tasks
    print "LIST OF TASKS:"
    mapM_ print utc
    mainLoop tasks
   else if (userInput) == "5" then do
    time <- getCurrentTime
    print time
    mainLoop tasks
   else if (userInput) == "6" then putStrLn "Exiting"
   else mainLoop tasks
 
main :: IO()
main = do
  let tasks = []
  mainLoop tasks
  