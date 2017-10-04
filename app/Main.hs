{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad.Logger
import Data.Aeson.TH (deriveFromJSON)
import Data.Data
import Data.Monoid
import Database.Persist.Postgresql
import Database.PostgreSql.Migrate.CLI
import Options.Applicative
import System.ConfigApp

-- | Minimal configuration file
data MigrateConfig = MigrateConfig {
  migrateDb     :: PostgresConf
, migrateFolder :: ConfigPath
} deriving (Data)
$(deriveFromJSON defaultOptions ''MigrateConfig)

main :: IO ()
main = runStdoutLoggingT . runMigrateOptions name migrateDb (unConfigPath . migrateFolder)
  =<< execParser opts
  where
    name = "pg-mmigrage"
    fullName = "PostgreSQL manual migrate"
    opts = info (helper <*> migrateOptionsParser)
        ( fullDesc
        <> progDesc "Simple file based migrations for PostgreSQL"
        <> header (fullName <> " - pefrorm DB migrations")
        )
