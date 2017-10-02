-- | Defines chain of hand-made DB migrations.
--
-- * Current version tag is stored in DB
-- * Has new/up/down commands
-- * New command creates new sql files `migrations/<ver+1>_<name>.(up|down).sql`
-- * Up command applyies all pending migrations
-- * Down command reverses single migration
-- * Migrations in the same version but with different names are considerent as
-- independent.
-- * Show warning message if last version in `migrations` folder is less than version
-- in DB.
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Database.PostgreSql.Migrate(
    DBVersion
  , MigrationName
  , loadMigrations
  , migrateNew
  , migrateUp
  , migrateDown
  , migrateDumpAll
  , getDbMaxVersion
  , getDbVersion
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH
import Prelude
import System.Directory.Tree
import System.FilePath
import Text.Read (readMaybe)

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T

-- | Tag of DB version
type DBVersion = Int

-- | Migration human readable name
type MigrationName = Text

share [mkPersist sqlSettings
     , mkMigrate "migrateMigrations"] [persistLowerCase|
CustomMigration sql=migrations
  version     DBVersion
  name        MigrationName
  up          Text
  down        Text
  applied     UTCTime Maybe
  UniqueMigration version name
|]

-- | Read all migrations from given folder and store them in table.
loadMigrations :: MonadIO m => Bool -> FilePath -> SqlPersistT m ()
loadMigrations forceUpdate dir = do
  runMigration migrateMigrations
  ms <- readMigrations dir
  ms' <- traverse (\m -> (m,) <$> loadMigration forceUpdate m) ms
  liftIO $ F.forM_ ms' $ \(CustomMigration{..}, status) -> putStrLn $ "Found migration "
    ++ show customMigrationVersion ++ "_" ++ unpack customMigrationName
    ++ ": " ++ unpack (printMigrationStatus status)

-- | Possible results of migration loading
data MigrationStatus = NewMigration -- ^ Migration is not known and loaded to DB
  | AppliedMigration -- ^ Migration is known and applied to DB
  | AppliedDiffers -- ^ Migration is known and applied to DB, but file version is differs from DB version
  | AppliedUpdate -- ^ Migration is known and aplied to DB, DB version was adjust to match file version
  | OverrideMigration -- ^ Migration is known and not applied to DB (migration file overrides contents of DB record)
  deriving (Eq, Ord, Show)

-- | Human readable label for status
printMigrationStatus :: MigrationStatus -> Text
printMigrationStatus ms = case ms of
  NewMigration -> "new"
  AppliedMigration -> "applied"
  AppliedDiffers -> "applied (file differs from db version)"
  AppliedUpdate -> "applied (updated to match file version)"
  OverrideMigration -> "override"

-- | Upload migration to DB
loadMigration :: MonadIO m => Bool -> CustomMigration -> SqlPersistT m MigrationStatus
loadMigration forceUpdate migr = do
  mr <- getBy $ UniqueMigration (customMigrationVersion migr) (customMigrationName migr)
  case mr of
    Nothing -> do
      _ <- insert migr
      pure NewMigration
    Just (Entity i dbmigr) -> case customMigrationApplied dbmigr of
      Nothing -> do
        repsert i migr
        pure OverrideMigration
      Just _ -> if customMigrationUp migr /= customMigrationUp dbmigr || customMigrationDown migr /= customMigrationDown dbmigr then
          if forceUpdate then do
            update i [CustomMigrationUp =. customMigrationUp migr, CustomMigrationDown =. customMigrationDown migr]
            pure AppliedUpdate
            else pure AppliedDiffers
        else pure AppliedMigration

-- | Create new migration and write down two files <version+1>_<name>.(up|down).sql to given folder.
-- The function doesn't write the new migration into DB.
migrateNew :: MonadIO m => FilePath -> MigrationName -> Bool -> SqlPersistT m [FilePath]
migrateNew migrationsFolder migrationName migrationNewVersion = do
  loadMigrations False migrationsFolder
  curVersion <- getDbMaxVersion
  let version = if migrationNewVersion then curVersion+1 else curVersion
  migrateDump migrationsFolder CustomMigration {
      customMigrationVersion = version
    , customMigrationName = migrationName
    , customMigrationUp = "BEGIN;\n-- Write here code to apply migration\nCOMMIT;"
    , customMigrationDown = "BEGIN;\n-- Write here code to reverse migration\nCOMMIT;"
    , customMigrationApplied = Nothing
    }

-- | Write down all migrations in DB into directory
migrateDumpAll :: MonadIO m => FilePath -> SqlPersistT m ()
migrateDumpAll migrationsFolder = do
  ms <- fmap entityVal <$> selectList [] []
  F.traverse_ (migrateDump migrationsFolder) ms

-- | Write down migration to files and return pathes to the files
migrateDump :: MonadIO m => FilePath -> CustomMigration -> SqlPersistT m [FilePath]
migrateDump migrationsFolder CustomMigration{..} = do
  let migrationName = show customMigrationVersion ++ "_" ++ unpack customMigrationName
      upName = migrationName ++ ".up.sql"
      downName = migrationName ++ ".down.sql"
  t <- liftIO $ writeDirectory $ "." :/ Dir migrationsFolder [
      File upName "BEGIN;\n-- Write here code to apply migration\nCOMMIT;"
    , File downName "BEGIN;\n-- Write here code to reverse migration\nCOMMIT;"
    ]
  pure $ fmap fst . F.toList . zipPaths $ t

-- | Apply all migrations from next version and return count of versions that are
-- not applied.
migrateUp :: (MonadIO m, MonadCatch m) => Int -> SqlPersistT m Int
migrateUp steps = do
  n <- getDbVersion
  go (n+1) steps
  curn <- getDbVersion
  nmax <- getDbMaxVersion
  pure $ nmax-curn
  where
    go _ 0 = pure ()
    go k n = do
      liftIO $ putStrLn $ "Applying migrations for version " ++ show k ++ "..."
      ms <- getVersionMigrations k
      reses <- traverse applyMigration $ filter (isNothing . customMigrationApplied . entityVal) ms
      if and reses then go (k+1) (n-1) else pure ()

-- | Reverse all migrations from current version and return count of versions
-- that are not applied. The step number refers to number of versions that are
-- fully applied, partial ones are not in count.
--
-- Note: to reverse partial applied migrations specify number of steps equal to 0.
migrateDown :: (MonadIO m, MonadCatch m) => Int -> SqlPersistT m Int
migrateDown steps = do
  n <- getDbVersion
  go (n+1) (steps+1)
  curn <- getDbVersion
  nmax <- getDbMaxVersion
  pure $ nmax-curn
  where
    go _ 0 = pure ()
    go k n = do
      liftIO $ putStrLn $ "Reversing migrations for version " ++ show k ++ "..."
      ms <- getVersionMigrations k
      reses <- traverse reverseMigration $ filter (isJust . customMigrationApplied . entityVal) ms
      if and reses then go (k-1) (n-1) else pure ()

-- | Get all migrations that are registered for given DB version
getVersionMigrations :: MonadIO m => DBVersion -> SqlPersistT m [Entity CustomMigration]
getVersionMigrations i = selectList [CustomMigrationVersion ==. i] [Asc CustomMigrationName]

-- | Apply action of migration and (if successfull, write down time of application)
applyMigration :: (MonadIO m, MonadCatch m) => Entity CustomMigration -> SqlPersistT m Bool
applyMigration (Entity i m@CustomMigration{..}) = do
  let migrationName = show customMigrationVersion ++ "_" ++ unpack customMigrationName
  liftIO . putStrLn $ "Appyling " ++ migrationName
  let exec = rawExecute customMigrationUp [] >> pure True
      handleFail (e :: SomeException) = do
        liftIO . putStrLn $ "Failed to apply migration "
          ++ migrationName ++ ": " ++ show e
        pure False
  res <- catch exec handleFail
  when res $ do
    t <- liftIO getCurrentTime
    repsert i m { customMigrationApplied = Just t }
  pure res

-- | Apply action of migration and (if successfull, wipe out time of application)
reverseMigration :: (MonadIO m, MonadCatch m) => Entity CustomMigration -> SqlPersistT m Bool
reverseMigration (Entity i m@CustomMigration{..}) = do
  let migrationName = show customMigrationVersion ++ "_" ++ unpack customMigrationName
  liftIO . putStrLn $ "Reversing " ++ migrationName
  let exec = rawExecute customMigrationDown [] >> pure True
      handleFail (e :: SomeException) = do
        liftIO . putStrLn $ "Failed to reverse migration "
          ++ migrationName ++ ": " ++ show e
        pure False
  res <- catch exec handleFail
  when res $ repsert i m { customMigrationApplied = Nothing }
  pure res

-- | Get version of last knowing migration
getDbMaxVersion :: MonadIO m => SqlPersistT m DBVersion
getDbMaxVersion = do
  mr <- selectFirst [] [Desc CustomMigrationVersion]
  pure . maybe 0 (customMigrationVersion . entityVal) $ mr

-- | Get current version of DB where all migrations are applied
getDbVersion :: MonadIO m => SqlPersistT m DBVersion
getDbVersion = do
  mr <- selectFirst [CustomMigrationApplied ==. Nothing] [Asc CustomMigrationVersion]
  maxv <- getDbMaxVersion
  pure . maybe maxv ((\a -> a - 1) . customMigrationVersion . entityVal) $ mr

-- | Temp data for merging migration files into full migration
data PartialMigration =
    MigrationUp DBVersion MigrationName Text
  | MigrationDown DBVersion MigrationName Text
  deriving (Eq, Ord)

-- | Get version from partial migration
partialVersion :: PartialMigration -> DBVersion
partialVersion = \case
  MigrationUp v _ _ -> v
  MigrationDown v _ _ -> v

-- | Get version from partial migration
partialName :: PartialMigration -> MigrationName
partialName = \case
  MigrationUp _ v _ -> v
  MigrationDown _ v _ -> v

-- | Read migrations from folder
readMigrations :: MonadIO m => FilePath -> m [CustomMigration]
readMigrations dir = do
  t <- liftIO $ zipPaths <$> readDirectoryWith T.readFile dir
  let makePartial (path, cnt) = do
        (version, name, mdir) <- parseMigrationName $ takeFileName path
        pure $ if mdir then MigrationUp version name cnt else MigrationDown version name cnt
      partials = catMaybes . fmap makePartial . F.toList $ t

      partialMap :: Map (Int, Text) (Either CustomMigration PartialMigration)
      partialMap = M.fromListWith mergeMigrations $ (\pm -> ((partialVersion pm, partialName pm), Right pm)) <$> partials

      mergeMigrations :: Either CustomMigration PartialMigration -> Either CustomMigration PartialMigration -> Either CustomMigration PartialMigration
      mergeMigrations (Right (MigrationUp version name cntUp)) (Right (MigrationDown _ _ cntDown)) = Left $ CustomMigration version name cntUp cntDown Nothing
      mergeMigrations (Right (MigrationDown version name cntDown)) (Right (MigrationUp _ _ cntUp)) = Left $ CustomMigration version name cntUp cntDown Nothing
      mergeMigrations m _ = m

      fullMigrations :: [CustomMigration]
      fullMigrations = F.toList $ M.mapMaybe (either Just (const Nothing)) partialMap

      failedMerges :: [PartialMigration]
      failedMerges = F.toList $ M.mapMaybe (either (const Nothing) Just) partialMap

  liftIO $ F.forM_ failedMerges $ \case
    MigrationUp version name _ -> putStrLn $ "Missing down migration for version " ++ show version ++ " and name " ++ unpack name ++ ". Ingnoring."
    MigrationDown version name _ -> putStrLn $ "Missing up migration for version " ++ show version ++ " and name " ++ unpack name ++ ". Ingnoring."
  pure fullMigrations

-- | Parse migration file name and extract version, name and up|down features.
parseMigrationName :: String -> Maybe (DBVersion, MigrationName, Bool)
parseMigrationName str = do
  let (versionStr, left0) = break ('_' ==) str
  version <- readMaybe versionStr
  let (name, left1) = break ('.' ==) $ drop 1 left0
      (dirStr, left2) = break ('.' ==) $ drop 1 left1
  dir <- case dirStr of
    "up" -> pure True
    "down" -> pure False
    _ -> mzero
  guard $ left2 == ".sql" || left2 == ".plsql"
  pure (version, pack name, dir)
