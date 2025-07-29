module Main where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp
import Servant
import qualified Configuration.Dotenv as ENV
import qualified Network.HTTP.Client as NETWORK 
import RepositoryLib.Repository (migrateDB, initConnectionPool, dotenvConnstr)
import Pagamento.ApiLib.Api (PagamentoApi, pagamentoServidor)
import qualified Pagamento.ViewModelsLib.AppSettingsVM as APPS
import Pagamento.ViewModelsLib.AppSettingsVM (AppSettings (AppSettings))

proxyServidor :: Proxy PagamentoApi
proxyServidor = Proxy

dotenvExposedport :: IO Int
dotenvExposedport = do
  vars <- ENV.parseFile ".env"
  let port = fromMaybe "80" (lookup "RINHA_HASKELL_PORT" vars)
  let validatedPort = readMaybe port
  case validatedPort of
    Nothing        -> fail "Falha ao ler porta configurada"
    Just validPort -> return validPort

dotenvHeadServer :: [(String, String)] -> IO Bool
dotenvHeadServer vars = do
  let headServer = fromMaybe "1" (lookup "HEAD_SERVER" vars)
  let validatedHeadServer = (readMaybe headServer) :: Maybe Int
  case validatedHeadServer of
    Nothing              -> fail "Falha ao determinar se o servidor é o HEAD"
    Just validHeadServer -> return $ validHeadServer == 1

dotenvDefaultCaller :: [(String, String)] -> IO (String, Int)
dotenvDefaultCaller vars = do
  vars <- ENV.parseFile ".env"
  let hostname = lookup "DEFAULT_API_HOSTNAME" vars
  let port = (readMaybe $ fromMaybe "80" (lookup "DEFAULT_API_PORT" vars)) >>= readMaybe :: Maybe Int
  case hostname of
    Nothing -> fail "Falha ao determinar hostname para a API default"
    Just validHostname ->
      case port of
        Nothing -> fail "Falha ao determinar porta da API default"
        Just validPort -> return (validHostname, validPort)

dotenvFallbackCaller :: [(String, String)] -> IO (String, Int)
dotenvFallbackCaller vars = do
  vars <- ENV.parseFile ".env"
  let hostname = lookup "FALLBACK_API_HOSTNAME" vars
  let port = (readMaybe $ fromMaybe "80" (lookup "FALLBACK_API_PORT" vars)) :: Maybe Int
  case hostname of
    Nothing -> fail "Falha ao determinar hostname para a API fallback"
    Just validHostname ->
      case port of
        Nothing -> fail "Falha ao determinar porta da API fallback"
        Just validPort -> return (validHostname, validPort)

appSettings :: IO AppSettings
appSettings = do
  vars <- ENV.parseFile ".env"
  headServer <- dotenvHeadServer vars
  defaultCaller <- dotenvDefaultCaller vars
  fallbackCaller <- dotenvFallbackCaller vars
  return $ AppSettings
    { APPS.headServer = headServer
    , APPS.defaultHostname = fst defaultCaller
    , APPS.defaultPort = snd defaultCaller
    , APPS.fallbackHostname = fst fallbackCaller
    , APPS.fallbackPort = snd fallbackCaller
    }

main :: IO ()
main = do
  connstr <- dotenvConnstr
  exposedport <- dotenvExposedport

  appSettings' <- appSettings

  pool <- initConnectionPool connstr
  manager' <- liftIO (NETWORK.newManager NETWORK.defaultManagerSettings)
  migrateDB (APPS.headServer appSettings') connstr
  run exposedport (serve proxyServidor $ pagamentoServidor pool manager' appSettings')
