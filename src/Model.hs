{-
    author: Japheth Adhavan
    author: Jason St. George
-}

{-# LANGUAGE
    OverloadedStrings
  , EmptyDataDecls
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , DeriveGeneric
  , GADTs
  , TypeFamilies
  , TemplateHaskell
  , QuasiQuotes
  , FlexibleInstances
  , FlexibleContexts
  , StandaloneDeriving #-}

module Model(
      Student(..)
    , Course(..)
    , CoursePrequisites(..)
    , StudentCourses(..)
    , entityDefs
    , EntityField(..)
) where

-- Needed for encoding and decoding to/from JSON

import GHC.Generics
import Data.Aeson
import Data.Default.Class

-- Needed for generating our model entities

import Database.Persist
import Database.Persist.Class
import Database.Persist.TH

share [mkPersist sqlSettings, mkSave "entityDefs", mkDeleteCascade sqlSettings][persistLowerCase|
  Student json
    firstname          String
    lastname           String
    email              String
    status             String
    UniqueEmail        email
    deriving Show Generic
  Course json
    title              String
    code               String
    department         String
    numberOfCredits    Int 
    UniqueCode         code
    deriving Show Generic
  CoursePrequisites
    courseId           CourseId
    prereqId           CourseId   Maybe default=NULL
  StudentCourses
    studentId          StudentId
    courseId           CourseId      Maybe default=NULL
    grade              String
  
|]