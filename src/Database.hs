{-
    author: Japheth Adhavan
    author: Jason St. George
-}

{-# LANGUAGE OverloadedStrings #-} -- Language Extensions


module Database (
    dbMigration
  , getStudents
  , getStudentById
  , insertStudent
  , updateStudentById
  , deleteStudentById
  , getCourses
  , getCourseById
  , insertCourse
  , updateCourseById
  , deleteCourseById
) where


import Model
import View

import System.Environment -- To get the DB connection string

import Data.Int
import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy

import Data.Maybe

import Data.Aeson

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control
import Control.Monad.Logger

import Database.Persist
import Database.Persist.Class
import Database.Persist.Sqlite as DbSql


-- Gather the database connection string from the environment
-- If not set use the default

sqliteConnString :: IO Data.Text.Text
sqliteConnString = do
  maybeDbConnString <- lookupEnv "MYCOURSES_DB_CONN"
  return $ Data.Text.pack $ fromMaybe "mycourses_clone_default.db" maybeDbConnString

withDbRun :: SqlPersistT (NoLoggingT (ResourceT IO)) b -> IO b
withDbRun command = do
    connString <- sqliteConnString
    runSqlite connString command

-- This will create our model tables if it does not already exist
-- Persistent will assist with update our table schema should our model change

dbMigration :: IO ()
dbMigration = do
    withDbRun $ runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Student)
    withDbRun $ runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Course)
    withDbRun $ runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe CoursePrequisite)
    withDbRun $ runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe StudentCourse)


{------------------------------------------------------------------------------------------}
-- Start Students
{------------------------------------------------------------------------------------------}

-- Helper function to convert the URL ID string to the needed 64 bit integer primary key
getStudentIdKey :: Maybe Data.ByteString.ByteString -> Key Student
getStudentIdKey maybeIdBS = toSqlKey studentIdInt64
    where
        studentIdBS = fromMaybe ("-1" :: Data.ByteString.ByteString) maybeIdBS
        studentIdInt64 = read (Data.ByteString.Char8.unpack studentIdBS) :: Int64

insertStudent :: Student -> IO (Key Student)
-- Create a new Student row in the database
insertStudent student = withDbRun $ DbSql.insert student

getStudents :: Maybe Data.ByteString.ByteString -> Maybe Data.ByteString.ByteString -> IO [Entity Student]
getStudents maybeLimitTo maybeOffsetBy = do
  -- If the limit and offset are `Nothing`, we will use the defaults 10 for the limit and 0 for the offset
  let limitToBS  = fromMaybe ("10" :: Data.ByteString.ByteString) maybeLimitTo
  let offsetByBS = fromMaybe ("0" :: Data.ByteString.ByteString) maybeOffsetBy
  -- Converts the strings to integers
  let limitToInt  = read (Data.ByteString.Char8.unpack limitToBS) :: Int
  let offsetByInt = read (Data.ByteString.Char8.unpack offsetByBS) :: Int
  -- The actual database call
  withDbRun $ DbSql.selectList ([] :: [Filter Student]) [LimitTo limitToInt, OffsetBy offsetByInt]


getStudentById :: Maybe Data.ByteString.ByteString -> IO (Key Student, Maybe Student)
getStudentById maybeIdBS = do
    -- Get the student primary key
    let studentIdKey = getStudentIdKey maybeIdBS
    -- Retrieve the student from the database
    maybeStudent <- withDbRun $ DbSql.get studentIdKey
    -- Return both the primary key and maybe the student (if it actually exists in the database)
    return (studentIdKey, maybeStudent)


updateStudentById :: Maybe Data.ByteString.ByteString -> StudentJSON -> IO (Key Student, Maybe Student)
updateStudentById maybeIdBS studentJSON = do
    let studentIdKey = getStudentIdKey maybeIdBS
    -- Look up the student in the database
    (studentKeyId, maybeStudent) <- getStudentById maybeIdBS
    case maybeStudent of
        -- If the student does not exist, return `Nothing`
        Nothing -> return (studentKeyId, Nothing)
        -- If the student does exist
        Just student -> do
            -- Create an updated student record
            let studentUpdated = Student {
                studentFirstname = fromMaybe (studentFirstname student) (studentJSONFirstname studentJSON),
                studentLastname  = fromMaybe (studentLastname student) (studentJSONLastname studentJSON),
                studentEmail     = fromMaybe (studentEmail student) (studentJSONEmail studentJSON),
                studentYear      = fromMaybe (studentYear student) (studentJSONYear studentJSON)
            }
            -- Update the student's fields in the database
            withDbRun $ DbSql.update studentKeyId [
                    StudentFirstname =. studentFirstname studentUpdated,
                    StudentLastname  =. studentLastname studentUpdated,
                    StudentEmail     =. studentEmail studentUpdated,
                    StudentYear      =. studentYear studentUpdated
                ]
            return (studentKeyId, Just studentUpdated)

deleteStudentById :: Maybe Data.ByteString.ByteString -> IO (Key Student, Maybe Student)
deleteStudentById maybeIdBS = do
    let studentIdKey = getStudentIdKey maybeIdBS
    -- Look up the student in the database
    (studentKeyId, maybeStudent) <- getStudentById maybeIdBS
    case maybeStudent of
        -- No student?
        Nothing -> return (studentKeyId, Nothing)
        -- Student?
        Just student -> do
            -- Delete the student from the database
            withDbRun $ DbSql.delete studentKeyId
            return (studentKeyId, Just student)

{------------------------------------------------------------------------------------------}
-- End Students
{------------------------------------------------------------------------------------------}


{------------------------------------------------------------------------------------------}
-- Courses
{------------------------------------------------------------------------------------------}

getCourseIdKey :: Maybe Data.ByteString.ByteString -> Key Course
getCourseIdKey maybeIdBS = toSqlKey courseIdInt64
    where
        courseIdBS  = fromMaybe ("-1" :: Data.ByteString.ByteString) maybeIdBS
        courseIdInt64 = read (Data.ByteString.Char8.unpack courseIdBS) :: Int64

insertCourse :: Course -> IO (Key Course)
insertCourse course = withDbRun $ DbSql.insert course

getCourses :: Maybe Data.ByteString.ByteString -> Maybe Data.ByteString.ByteString -> IO [Entity Course]
getCourses maybeLimitTo maybeOffsetBy = do
  let limitToBS   = fromMaybe ("10" :: Data.ByteString.ByteString) maybeLimitTo
  let offsetByBS  = fromMaybe ("0" :: Data.ByteString.ByteString) maybeOffsetBy
  let limitToInt  = read (Data.ByteString.Char8.unpack limitToBS) :: Int
  let offsetByInt = read (Data.ByteString.Char8.unpack offsetByBS) :: Int
  withDbRun $ DbSql.selectList ([] :: [Filter Course]) [LimitTo limitToInt, OffsetBy offsetByInt]


getCourseById :: Maybe Data.ByteString.ByteString -> IO (Key Course, Maybe Course)
getCourseById maybeIdBS = do
    let courseIdKey = getCourseIdKey maybeIdBS
    maybeCourse <- withDbRun $ DbSql.get courseIdKey
    return (courseIdKey, maybeCourse)


updateCourseById :: Maybe Data.ByteString.ByteString -> CourseJSON -> IO (Key Course, Maybe Course)
updateCourseById maybeIdBS courseJSON = do
    let courseIdKey = getCourseIdKey maybeIdBS
    (courseKeyId, maybeCourse) <- getCourseById maybeIdBS
    case maybeCourse of
        Nothing -> return (courseKeyId, Nothing)
        Just course -> do
            let courseUpdated = Course {
                courseTitle      = fromMaybe (courseTitle course) (courseJSONTitle courseJSON),
                courseCode       = fromMaybe (courseCode course) (courseJSONCode courseJSON),
                courseDepartment = fromMaybe (courseDepartment course) (courseJSONDepartment courseJSON),
                courseCredits    = fromMaybe (courseCredits course) (courseJSONCredits courseJSON)
            }
            withDbRun $ DbSql.update courseKeyId [
                    CourseTitle      =. courseTitle courseUpdated,
                    CourseCode       =. courseCode courseUpdated,
                    CourseDepartment =. courseDepartment courseUpdated,
                    CourseCredits    =. courseCredits courseUpdated
                ]
            return (courseKeyId, Just courseUpdated)


deleteCourseById :: Maybe Data.ByteString.ByteString -> IO (Key Course, Maybe Course)
deleteCourseById maybeIdBS = do
    let courseIdKey = getCourseIdKey maybeIdBS
    (courseKeyId, maybeCourse) <- getCourseById maybeIdBS
    case maybeCourse of
        Nothing -> return (courseIdKey, Nothing)
        Just course -> do
            withDbRun $ DbSql.delete courseKeyId
            return (courseKeyId, Just course)
