{-
    author: Japheth Adhavan
    author: Jason St. George
-}

{-# LANGUAGE OverloadedStrings #-}

module Controller (
    mainRouter
) where


import Database
import Model
import View
import Snap
import Data.ByteString
import Data.ByteString.Lazy
import Data.Maybe
import Data.Aeson
import Control.Monad.IO.Class (liftIO)
import Database.Persist
    

-- Here is a top level router
-- This will define the base and other routes

mainRouter :: Snap ()
mainRouter =  route [
                  (    "", writeBS "") -- Base / route
                , ("students", studentsRouter) -- /students route
              ]
              

studentsRouter :: Snap ()
studentsRouter =  route [
                  (    "", method GET    studentsRouteIndex)  -- Gets a list of students
                , (    "", method POST   studentsRouteCreate) -- Creates a new student
                , ("/:id", method GET    studentsRouteShow)   -- Gets a single student by /:id
                , ("/:id", method PUT    studentsRouteUpdate) -- Updates a single student by /:id
                , ("/:id", method DELETE studentsRouteDelete) -- Deletes a single student by /:id
            ]

set404AndContentType :: Snap ()
set404AndContentType = do
    -- Set the HTTP status code to 404 (not found)
    modifyResponse $ setResponseCode 404
    -- Set the content type as JSON
    -- This will let the client know what kind of data is being returned
    -- in the HTTP response body
    modifyResponse $ setHeader "Content-Type" "application/json"

    
studentsRouteIndex :: Snap ()
studentsRouteIndex = do
    -- Get the limit and start paramters (?limit=:limit&start=:start) if sent
    maybeLimitTo  <- getParam "limit"
    maybeOffsetBy <- getParam "start"
    -- Get a list or array of students from the database
    students <- liftIO $ getStudents maybeLimitTo maybeOffsetBy
    -- Set the content type to JSON
    -- We will be responding with JSON
    modifyResponse $ setHeader "Content-Type" "application/json"
    -- Write out the JSON response
    writeLBS $ encode $ Prelude.map entityIdToJSON students

studentsRouteShow :: Snap ()
studentsRouteShow = do
    -- We will start off assuming the student could not be found
    -- This sets the HTTP status code to 404 (not found)
    set404AndContentType
    -- Get the ID parameter
    maybeStudentId <- getParam "id"
    -- Get the student primary key and record
    (studentIdKey, maybeStudent) <- liftIO $ getStudentById maybeStudentId
    -- Respond with 200 if the student with ID actually exists
    -- This will write out our JSON response
    resposndWithMaybeStudent 200 studentIdKey maybeStudent

studentsRouteCreate :: Snap ()
studentsRouteCreate = do
    -- Read in the request HTTP body
    body <- readRequestBody 50000
    -- Parse the JSON request body into a `Student` model (record)
    let student = studentJSONToStudent $ parseBodyToStudentJSON body
    -- Insert the student into the database
    studentIdKey <- liftIO $ insertStudent student
    -- Set the content type to JSON
    modifyResponse $ setHeader "Content-Type" "application/json"
    -- Let the client know that we created a new record (201)
    -- Respond with the newly created student in JSON format
    respondWithStudent 201 studentIdKey student

studentsRouteUpdate :: Snap ()
studentsRouteUpdate = do
    set404AndContentType
    maybeStudentId <- getParam "id"
    body <- readRequestBody 50000
    -- Parse the request body into `StudentJSON`
    let studentJSON = parseBodyToStudentJSON body
    -- Update the student if it exists
    (studentIdKey, maybeStudent) <- liftIO $ updateStudentById maybeStudentId studentJSON
    -- If the student exists, tell the client OK (200)
    -- Respond with the student JSON or an error message in JSON
    resposndWithMaybeStudent 200 studentIdKey maybeStudent

studentsRouteDelete :: Snap ()
studentsRouteDelete = do
    set404AndContentType
    maybeStudentId <- getParam "id"
    -- Delete the student in the database if it exists
    (studentIdKey, maybeStudent) <- liftIO $ deleteStudentById maybeStudentId
    -- If the student exists, resond with 200 and the student in JSON form
    -- Otherwise respond with 404 (not found) and an error message in JSON format
    resposndWithMaybeStudent 200 studentIdKey maybeStudent

parseBodyToStudentJSON :: Data.ByteString.Lazy.ByteString -> StudentJSON
-- Parse a raw HTTP body into a `StudentJSON` record
parseBodyToStudentJSON body = fromMaybe (StudentJSON (Just "") (Just "") (Just "") (Just "")) (decode body :: Maybe StudentJSON)

resposndWithMaybeStudent :: Int -> Key Student -> Maybe Student -> Snap()
resposndWithMaybeStudent code studentIdKey maybeStudent = case maybeStudent of
    -- Student not found?
    Nothing -> writeBS ("{\"error\": \"Not found.\"}" :: Data.ByteString.ByteString)
    -- Student found?
    -- The code is the HTTP status code
    Just student -> respondWithStudent code studentIdKey student

respondWithStudent :: Int -> Key Student -> Student -> Snap()
respondWithStudent code studentIdKey student = do
    -- Set the HTTP status code
    modifyResponse $ setResponseCode code
    -- Write out the student in JSON format into the response body
    writeLBS $ studentAsJSONLBS studentIdKey student