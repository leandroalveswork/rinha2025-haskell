{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module RepositoryLib.Repository
  ( migrateDB
  , initConnectionPool
  , DBConnectionString
  , dotenvConnstr
  ) where

import Data.ByteString             (ByteString)
import Data.String                 (IsString(fromString))
import Control.Exception           (bracket)

import qualified System.IO as SIO
import qualified Data.List as DL
import qualified Database.PostgreSQL.Simple as SQL
import qualified Data.Pool as DP
import qualified Configuration.Dotenv as ENV

type DBConnectionString = ByteString

getFileLines :: String -> IO [String]
getFileLines fileName = do
  dataMassFile <- SIO.openFile ("src/RepositoryLib/" ++ fileName) SIO.ReadMode
  dataMass <- SIO.hGetContents dataMassFile
  return (lines dataMass)

getFileContentAsSql :: String -> IO SQL.Query
getFileContentAsSql = fmap ((fromString  . DL.intercalate " ")) . getFileLines

migrateDB :: Bool -> DBConnectionString -> IO ()
migrateDB headserver connstr = if headserver then _migrateDB connstr else return ()

_migrateDB :: DBConnectionString -> IO ()
_migrateDB connstr = bracket (SQL.connectPostgreSQL $ connstr) SQL.close $ \conn -> do
  versionManagerQuery <- getFileContentAsSql "VERSION_MANAGER.sql"
  versions <- fmap (map SQL.fromOnly)
    $ ((SQL.query_ conn versionManagerQuery)::(IO [SQL.Only String]))
  (if "START" `elem` versions
      then return ()
      else do
        startQuery <- getFileContentAsSql "START.sql"
        _ <- SQL.execute_ conn startQuery
        return ())

initConnectionPool :: DBConnectionString -> IO (DP.Pool SQL.Connection)
initConnectionPool connStr =
  let poolConf :: DP.PoolConfig SQL.Connection
      poolConf = DP.setNumStripes 
        (Just 5)
        $ DP.defaultPoolConfig 
            (SQL.connectPostgreSQL connStr)
            SQL.close
            30 -- conexões são mantidas abertas até 30s de inatividade
            20 -- max. 4 conexões abertas por stripe
  in DP.newPool poolConf

dotenvConnstr :: IO DBConnectionString
dotenvConnstr = do
  vars <- ENV.parseFile ".env"
  (let connstr :: Maybe DBConnectionString
       connstr = fmap fromString $ lookup "POSTGRESQL_CONNSTR" vars
   in case connstr of
        Nothing -> fail "Falha ao ler as variáveis de conexão com banco de dados"
        Just okConnstr -> return okConnstr)
