{-
    author: Japheth Adhavan
    author: Jason St. George
-}

{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric #-}

module View (
    StudentJSON(..),
    studentJSONToStudent,
    studentAsJSONLBS
) where


import Model


-- Build dependencies

import GHC.Generics
import Data.Int
import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy
import Data.Maybe
import Data.Aeson
import Data.Default.Class
import Database.Persist
import Database.Persist.Class

-- Our "view"

data CourseGrade = CourseGrade {
    courseTitle :: String,
    grade  :: String
} deriving (Show, Generic)

data StudentJSON = StudentJSON {
    studentJSONFirstname :: Maybe String,
    studentJSONLastname  :: Maybe String,
    studentJSONEmail     :: Maybe String,
    studentJSONYear      :: Maybe String
} deriving (Show, Generic)


data CoursesJSON = CoursesJSON {
    title      :: Maybe String,
    code       :: Maybe String,
    department :: Maybe String,
    credits    :: Maybe Int
} deriving (Show, Generic)


data CoursePrerequisiteJSON = CoursePrerequisiteJSON {
    courseId     :: Maybe String,
    prereq       :: Maybe [String]
} deriving (Show, Generic)

data StudentCoursesJSON = StudentCoursesJSON {
    studentId           :: Maybe Int,
    courseGradeList     :: Maybe [CourseGrade]
} deriving (Show, Generic)


instance FromJSON StudentJSON where
    parseJSON (Object v) =
        StudentJSON <$> v .:?  "firstname"
                    <*> v .:?  "lastname"
                    <*> v .:?  "email"
                    <*> v .:?  "year"

instance ToJSON StudentJSON where
    toJSON (StudentJSON fname lname email yr) = object ["firstname" .= fname, 
                                                        "lastname"  .= lname,
                                                        "email"     .= email,
                                                        "year"      .= yr]

studentJSONToStudent :: StudentJSON -> Student
studentJSONToStudent studentJSON = Student fname lname eml yr
    where fname = fromMaybe "" $ studentJSONFirstname studentJSON
          lname = fromMaybe "" $ studentJSONLastname studentJSON
          eml   = fromMaybe "" $ studentJSONEmail studentJSON
          yr    = fromMaybe "" $ studentJSONYear studentJSON

studentAsJSONLBS :: Key Student -> Student -> Data.ByteString.Lazy.ByteString
studentAsJSONLBS k s = encode . entityIdToJSON $ Entity k s
                                                        