{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module MigrationSpec(
    spec
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader
import Data.Aeson.TH (deriveFromJSON)
import Data.Data
import Data.Foldable (traverse_)
import Data.Monoid
import Data.Text (Text, intercalate)
import Data.Yaml.Config
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.PostgreSql.Migrate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Test.HUnit as HUnit
import System.ConfigApp
import System.IO.Temp
import Test.Hspec
import Text.Shakespeare.Text (st)
import Text.Show.Pretty

-- | Migration inmemory description
data MDescr = MDescr {
  testVersion :: Int
, testName    :: FilePath
, testUp      :: Text
, testDown    :: Text
}

data MigrateConfig = MigrateConfig {
  migrateDb     :: PostgresConf
, migrateFolder :: ConfigPath
} deriving (Data)
$(deriveFromJSON defaultOptions ''MigrateConfig)

data TestData = TestData {
  testPool :: ConnectionPool
}

runDB :: TestData -> SqlPersistM a  -> IO a
runDB td q = runSqlPersistMPool q $ testPool td

runDBWithApp :: TestData -> SqlPersistM a -> IO a
runDBWithApp TestData{..} q = runSqlPersistMPool q testPool

createTestData :: MigrateConfig -> IO TestData
createTestData MigrateConfig{..} = fmap TestData $ runStdoutLoggingT $ createPostgresqlPool
    (pgConnStr migrateDb) (pgPoolSize migrateDb)


withApp :: SpecWith TestData -> Spec
withApp = before $ do
  config <- loadYamlSettings ["test-config.yaml"] [] useEnv
  tdata <- createTestData config
  wipeDB tdata
  wipeTestSchema tdata
  return tdata

-- This function will recreate (delete and create) 'test' schema.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeTestSchema :: TestData -> IO ()
wipeTestSchema app = runDBWithApp app $ do
  rawExecute "drop schema if exists test cascade;" []
  rawExecute "create schema test;" []

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: TestData -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        q = "TRUNCATE TABLE " <> intercalate ", " escapedTables
    unless (null escapedTables) $ rawExecute q []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ filter (not . T.null) $ map unSingle tables

-- | Write migration description to folder
writeMDescr :: FilePath -> MDescr -> IO ()
writeMDescr p d = do
  let baseName = p ++ "/" ++ show (testVersion d) ++ "_" ++ testName d
      nameUp = baseName ++ ".up.sql"
      nameDown = baseName ++ ".down.sql"
  T.writeFile nameUp $ testUp d
  T.writeFile nameDown $ testDown d

-- | Write all migrations to folder and pass the folder path to action
withMigrations :: (MonadIO m, MonadMask m) => [MDescr] -> (FilePath -> m a) -> m a
withMigrations ds f = withSystemTempDirectory "cyberpass_test_migrations" $ \name -> do
  liftIO $ traverse_ (writeMDescr name) ds
  f name

-- | Does the given table exists in the DB
tableExists :: MonadIO m => Text -> SqlPersistT m Bool
tableExists n = do
  v :: [Single Int] <-
    rawSql "select count(table_name) from information_schema.tables where table_schema='test' and table_name=?"  [PersistText n]
  pure $ v /= [Single 0]

-- | Asserts that the two given values are equal.
--
-- In case they are not equal, error mesasge includes the two values.
--
-- From yesod-test
assertEq :: (HasCallStack, Eq a, Show a, MonadIO m) => String -> a -> a -> m ()
assertEq m a b =
  liftIO $ HUnit.assertBool msg (a == b)
  where msg = "Assertion: " ++ m ++ "\n" ++
              "First argument:  " ++ ppShow a ++ "\n" ++
              "Second argument: " ++ ppShow b ++ "\n"

spec :: Spec
spec = withApp $ do
  describe "Apply simple migration" $ do
    it "can apply 0/1 up" $ \td -> withMigrations [
        MDescr 0 "a" "create table test.a();" "drop table test.a;"
      ]  $ \name -> do
        n <- runDB td $ do
          loadMigrations False name
          migrateUp 0
        assertEq "Migration steps left" n 1
        aExist <- runDB td $ tableExists "a"
        assertEq "Table a exist" aExist False
    it "can apply 1/1 up" $ \td -> withMigrations [
        MDescr 0 "a" "create table test.a();" "drop table test.a;"
      ]  $ \name -> do
        n <- runDB td $ do
          loadMigrations False name
          migrateUp 1
        assertEq "Migration steps left" n 0
        aExist <- runDB td $ tableExists "a"
        assertEq "Table a exist" aExist True
    it "can apply 1/2 up" $ \td -> withMigrations [
        MDescr 0 "a" "create table test.a();" "drop table test.a;"
      , MDescr 1 "b" "create table test.b();" "drop table test.b;"
      ]  $ \name -> do
        n <- runDB td $ do
          loadMigrations False name
          migrateUp 1
        assertEq "Migration steps left" n 1
        aExist <- runDB td $ tableExists "a"
        assertEq "Table a exist" aExist True
        bExist <- runDB td $ tableExists "b"
        assertEq "Table b exist" bExist False
    it "can apply 2/2 up" $ \td -> withMigrations [
        MDescr 0 "a" "create table test.a();" "drop table test.a;"
      , MDescr 1 "b" "create table test.b();" "drop table test.b;"
      ]  $ \name -> do
        n <- runDB td $ do
          loadMigrations False name
          migrateUp 2
        assertEq "Migration steps left" n 0
        aExist <- runDB td $ tableExists "a"
        assertEq "Table a exist" aExist True
        bExist <- runDB td $ tableExists "b"
        assertEq "Table b exist" bExist True
    it "can apply 0/1 down" $ \td -> withMigrations [
        MDescr 0 "a" "create table test.a();" "drop table test.a;"
      ]  $ \name -> do
        n <- runDB td $ do
          loadMigrations False name
          migrateDown 0
        assertEq "Migration steps left" n 1
        aExist <- runDB td $ tableExists "a"
        assertEq "Table a exist" aExist False
    it "can apply 1/1 down" $ \td -> withMigrations [
        MDescr 0 "a" "create table test.a();" "drop table test.a;"
      ]  $ \name -> do
        n <- runDB td $ do
          loadMigrations False name
          _ <- migrateUp 1
          migrateDown 1
        assertEq "Migration steps left" n 1
        aExist <- runDB td $ tableExists "a"
        assertEq "Table a exist" aExist False
    it "can apply 1/2 down" $ \td -> withMigrations [
        MDescr 0 "a" "create table test.a();" "drop table test.a;"
      , MDescr 1 "b" "create table test.b();" "drop table test.b;"
      ]  $ \name -> do
        n <- runDB td $ do
          loadMigrations False name
          _ <- migrateUp 2
          migrateDown 1
        assertEq "Migration steps left" n 1
        aExist <- runDB td $ tableExists "a"
        assertEq "Table a exist" aExist True
        bExist <- runDB td $ tableExists "b"
        assertEq "Table b exist" bExist False
    it "can apply 2/2 down" $ \td -> withMigrations [
        MDescr 0 "a" "create table test.a();" "drop table test.a;"
      , MDescr 1 "b" "create table test.b();" "drop table test.b;"
      ]  $ \name -> do
        n <- runDB td $ do
          loadMigrations False name
          _ <- migrateUp 2
          migrateDown 2
        assertEq "Migration steps left" n 2
        aExist <- runDB td $ tableExists "a"
        assertEq "Table a exist" aExist False
        bExist <- runDB td $ tableExists "b"
        assertEq "Table b exist" bExist False
