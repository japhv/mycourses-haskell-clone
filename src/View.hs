{-
    author: Japheth Adhavan
    author: Jason St. George
-}

{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric #-}

{-# OPTIONS -fno-warn-unused-imports #-}

module View (
    StudentJSON(..),
    CourseJSON(..),
    studentJSONToStudent,
    studentAsJSONLBS,
    courseJSONToCourse,
    courseAsJSONLBS,
    studentCoursesAsJSONLBS
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

data StudentCourseJSON = StudentCourseJSON {
    studentCourseJSONTitle :: String,
    studentCourseJSONCode :: String,
    studentCourseJSONDepartment :: String,
    studentCourseJSONCredits :: Int,
    studentCourseJSONGrade  :: Maybe String
} deriving (Show, Generic)

data StudentJSON = StudentJSON {
    studentJSONFirstname :: Maybe String,
    studentJSONLastname  :: Maybe String,
    studentJSONEmail     :: Maybe String,
    studentJSONYear      :: Maybe String
} deriving (Show, Generic)


data CourseJSON = CourseJSON {
    courseJSONTitle      :: Maybe String,
    courseJSONCode       :: Maybe String,
    courseJSONDepartment :: Maybe String,
    courseJSONCredits    :: Maybe Int
} deriving (Show, Generic)


data CoursePrerequisiteJSON = CoursePrerequisiteJSON {
    courseId     :: Maybe String,
    prereq       :: Maybe [String]
} deriving (Show, Generic)


data StudentCoursesJSON = StudentCoursesJSON {
    studentId           :: Maybe Int,
    courseGradeList     :: Maybe [StudentCourseJSON]
} deriving (Show, Generic)


{------------------------------------------------------------------------------------------}
-- Start Students
{------------------------------------------------------------------------------------------}

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

{------------------------------------------------------------------------------------------}
-- End Students
{------------------------------------------------------------------------------------------}

{------------------------------------------------------------------------------------------}
-- Start Courses
{------------------------------------------------------------------------------------------}

instance FromJSON CourseJSON where
  parseJSON (Object v) =
        CourseJSON  <$> v .:? "title"
                    <*> v .:? "code"
                    <*> v .:? "department"
                    <*> v .:? "credits"

instance ToJSON CourseJSON where
  toJSON (CourseJSON titl code dept crdt) = object ["title"       .= titl,
                                                     "code"       .= code,
                                                     "department" .= dept,
                                                     "credits"    .= crdt]

courseJSONToCourse :: CourseJSON -> Course
courseJSONToCourse coursesJSON = Course titl code dept crdt
    where titl = fromMaybe "" $ courseJSONTitle coursesJSON
          code = fromMaybe "" $ courseJSONCode coursesJSON
          dept = fromMaybe "" $ courseJSONDepartment coursesJSON
          crdt = fromMaybe  0 $ courseJSONCredits coursesJSON

courseAsJSONLBS :: Key Course -> Course -> Data.ByteString.Lazy.ByteString
courseAsJSONLBS k s = encode . entityIdToJSON $ Entity k s

{------------------------------------------------------------------------------------------}
-- End Courses
{------------------------------------------------------------------------------------------}


instance ToJSON StudentCourseJSON where
    toJSON (StudentCourseJSON titl code dept crdt grade) = object ["title"       .= titl,
                                                                    "code"       .= code,
                                                                    "department" .= dept,
                                                                    "credits"    .= crdt,
                                                                    "grade"      .= grade]


studentCoursesAsJSONLBS :: (Entity Course, Entity StudentCourse) -> StudentCourseJSON
studentCoursesAsJSONLBS  (course, studentCourse) = studCourse
    where studCourse = StudentCourseJSON {
        studentCourseJSONTitle = courseTitle $ entityVal course,
        studentCourseJSONCode = courseCode $ entityVal course,
        studentCourseJSONDepartment = courseDepartment $ entityVal course,
        studentCourseJSONCredits = courseCredits $ entityVal course,
        studentCourseJSONGrade = studentCourseGrade $ entityVal studentCourse
    }
