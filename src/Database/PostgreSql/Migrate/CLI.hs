-- | CLI utilities for migration actions
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.PostgreSql.Migrate.CLI(
    MigrateOptions(..)
  , MigrateCommand(..)
  , migrateOptionsParser
  , migrateCommandsParser
  , runMigrateOptions
  ) where

import Control.Monad
import Control.Monad.Logger
import Data.Aeson
import Data.Data
import Data.Data
import Data.Maybe
import Data.Monoid
import Data.Text (pack, unpack)
import Database.Persist.Postgresql
import Database.PostgreSql.Migrate
import GHC.Generics
import Options.Applicative
import System.ConfigApp
import System.Directory.Tree

import qualified Data.Foldable as F

-- | Holds path to config and migration command to perform
--
-- [@a@] - is type of config that would be parsed from YAML config in `mOptionsConfig`
data MigrateOptions a = MigrateOptions {
  mOptionsConfig  :: Maybe FilePath -- ^ Config path, default path is ~/.<ap>/prod/config.yaml
, mOptionsMigrationsFolder :: Maybe FilePath -- ^ Path to folder with migrations, if 'Nothing' use it from `a` config
, mOptionsCommand :: MigrateCommand -- ^ Migration command
}

-- | Actual migration command to perform
data MigrateCommand =
    -- | Create new migration template
    MigrateCommandNew {
      migrationName       :: MigrationName -- ^ Name of migration file
    , migrationNewVersion :: Bool -- ^ Allocate new version number? Or use the last known version.
    }
  -- | Apply all migrations to the next version number
  | MigrateCommandUp {
      migrationUpTo    :: Maybe Int -- ^ Upper bound
    , migrationUpSteps :: Maybe Int -- ^ How much versions to apply (defalt 1)
    , migrationUpAll   :: Bool -- ^ If set, migrate all versions that are known (migrationUpTo restricts the maximum version)
    }
  -- | Apply all migrations to previous version number
  | MigrateCommandDown {
      migrationDownTo    :: Maybe Int -- ^ Lower bound
    , migrationDownSteps :: Maybe Int -- ^ How much version to apply (default 1)
    , migrationDownAll   :: Bool -- ^ If set, tries to reverse all migrations down to migrationDownTo version
    }
  -- | Save migrations stored in DB state to local files
  | MigrateCommandDump
  -- | Save migrations from file to DB state, but not apply
  | MigrateCommandLoad {
      migrationForce :: Bool -- ^ If files contradicts DB state, force replacement
    }
  deriving (Eq, Ord, Show, Read, Generic, Data)

-- | Parse migration options from CLI arguments
migrateOptionsParser :: Parser (MigrateOptions a)
migrateOptionsParser = MigrateOptions
  <$> (optional . strOption) (
         long "conf"
      <> metavar "CONFIG"
      <> help "Configuration file"
    )
  <*> (optional . strOption) (
         long "folder"
      <> metavar "MIGRATIONS_FOLDER"
      <> help "Override location of migrations"
    )
  <*> migrateCommandsParser

-- | Parse migration command from CLI arguments
migrateCommandsParser :: Parser MigrateCommand
migrateCommandsParser = subparser $
       command "new" (info newArgs (progDesc "Generate new migrations files"))
    <> command "up" (info upArgs (progDesc "Apply migrations"))
    <> command "down" (info downArgs (progDesc "Reverse migrations"))
    <> command "dump" (info (pure MigrateCommandDump) (progDesc "Dump all migrations from DB to disk"))
    <> command "load" (info loadArgs (progDesc "Load migrations from files to DB and print info"))
  where
    newArgs = (helper <*>) $ MigrateCommandNew
      <$> (fmap pack . strArgument) (
           metavar "MIGRATION_NAME"
        <> help "Migration name unique across single version"
        )
      <*> switch (
           long "new-version"
        <> help "Allocate new version of DB"
        )
    upArgs = (helper <*>) $ MigrateCommandUp
      <$> (optional . option auto) (
           long "to"
        <> metavar "VERSION_NUMBER"
        <> help "Migrate to the given version number"
        )
      <*> (optional . option auto) (
           long "steps"
        <> metavar "NUMBER_OF_VERSIONS"
        <> help "Number of versions to migrage, default 1"
        )
      <*> switch (
           long "all"
        <> help "Migrate to latest known version"
        )
    downArgs = (helper <*>) $ MigrateCommandDown
      <$> (optional . option auto) (
           long "to"
        <> metavar "VERSION_NUMBER"
        <> help "Migrate to the given version number"
        )
      <*> (optional . option auto) (
           long "steps"
        <> metavar "NUMBER_OF_VERSIONS"
        <> help "Number of versions to migrage, default 1"
        )
      <*> switch (
           long "all"
        <> help "Migrate to earliest known version"
        )
    loadArgs = (helper <*>) $ MigrateCommandLoad
      <$> switch (
           long "force"
        <> help "Force update up/down scripts in DB"
        )

-- | Run migrateion commands from given options
runMigrateOptions :: (FromJSON a, Data a)
  => String -- ^ Name of app, used for default config location ~/.<name>/prod/config.yaml
  -> (a -> PostgresConf) -- ^ Whay to extract postgresql config from value
  -> (a -> FilePath) -- ^ Location of migrations folder
  -> MigrateOptions a -- ^ Options to run
  -> IO ()
runMigrateOptions name getPgConfig getMigrationsFolder MigrateOptions{..} = do
  cfg <- readConfigBy name mOptionsConfig
  -- Create the database connection pool
  let dbCfg = getPgConfig cfg
  pool :: ConnectionPool <- runStdoutLoggingT $ createPostgresqlPool
      (pgConnStr  dbCfg) (pgPoolSize dbCfg)
  let runDB = flip runSqlPool pool
      migrationsFolder = fromMaybe (getMigrationsFolder cfg) mOptionsMigrationsFolder
  case mOptionsCommand of
    MigrateCommandNew{..} -> do
      t <- runDB $ migrateNew migrationsFolder migrationName migrationNewVersion
      putStrLn "Modify the following files:"
      F.traverse_ putStrLn t
    MigrateCommandUp{..} -> do
      curn <- runDB getDbVersion
      maxn <- runDB getDbMaxVersion
      let steps = if migrationUpAll then maxn-curn
            else case migrationUpTo of
              Just n -> n - curn
              Nothing -> fromMaybe 1 migrationUpSteps
      n <- runDB $ migrateUp steps
      newCurN <- runDB getDbVersion
      putStrLn $ "Current version " ++ show newCurN
      putStrLn $ "You are now behind the head by " ++ show n ++ " versions"
    MigrateCommandDown{..} -> do
      curn <- runDB getDbVersion
      maxn <- runDB getDbMaxVersion
      let steps = if migrationDownAll then maxn
            else case migrationDownTo of
              Just n -> curn - n
              Nothing -> fromMaybe 1 migrationDownSteps
      n <- runDB $ migrateDown steps
      newCurN <- runDB getDbVersion
      putStrLn $ "Current version " ++ show newCurN
      putStrLn $ "You are now behind the head by " ++ show n ++ " versions"
    MigrateCommandDump -> do
      runDB $ migrateDumpAll migrationsFolder
      putStrLn "Done"
    MigrateCommandLoad{..} -> runDB $ loadMigrations migrationForce migrationsFolder
