{-
    author: Japheth Adhavan
    author: Jason St. George
-}

{-# LANGUAGE OverloadedStrings #-} -- Language Extensions


module Database where


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
  return $ Data.Text.pack $ fromMaybe "mycourses_vlone_default.db" maybeDbConnString


-- enableForeignKeys :: Sqlite.Connection -> IO ()
-- enableForeignKeys conn = Sqlite.prepare conn "PRAGMA foreign_keys = ON;" >>= void . Sqlite.step

withDbRun :: SqlPersistT (NoLoggingT (ResourceT IO)) b -> IO b
withDbRun command = do
    connString <- sqliteConnString
    -- conn <- Sqlite.open connString
    -- enableForeignKeys conn
    runSqlite connString command

-- This will create our model tables if it does not already exist
-- Persistent will assist with update our table schema should our model change

dbMigration :: IO ()
dbMigration = do
    withDbRun $ runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Student)
    withDbRun $ runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Course)
    withDbRun $ runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe CoursePrequisite)
    withDbRun $ runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe StudentCourse)


