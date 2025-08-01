module Main where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp
import Servant
import qualified Configuration.Dotenv as ENV
import qualified Network.HTTP.Client as NETWORK 
import RepositoryLib.Repository (migrateDB, initConnectionPool, dotenvConnstr)
import Pagamento.ApiLib.Api (pagamentoServidor)
import Pagamento.ApiLib.ApiType (PagamentoApi)
import qualified Pagamento.ViewModelsLib.AppSettingsVM as APPS
import Pagamento.ViewModelsLib.AppSettingsVM (AppSettings (AppSettings), Domain(Domain), dmHostname, dmPort)
import Control.Concurrent (newEmptyMVar)

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

dotenvDomain :: String -> [(String, String)] -> String -> IO Domain
dotenvDomain key vars errorTextArg = do
  let hostname = lookup (key ++ "_HOSTNAME") vars
  let port = (readMaybe $ fromMaybe "80" (lookup (key ++ "_PORT") vars)) :: Maybe Int
  case hostname of
    Nothing -> fail $ "Falha ao determinar hostname " ++ errorTextArg
    Just validHostname ->
      case port of
        Nothing -> fail $ "Falha ao determinar porta " ++ errorTextArg
        Just validPort -> return $ Domain { dmHostname = validHostname, dmPort = validPort }

dotenvHeadCaller :: [(String, String)] -> IO Domain
dotenvHeadCaller vars = dotenvDomain "HEAD" vars "para a API de Head"

dotenvHelper1Caller :: [(String, String)] -> IO Domain
dotenvHelper1Caller vars = dotenvDomain "HELPER_1" vars "para a API do Helper 1"

dotenvDefaultCaller :: [(String, String)] -> IO Domain
dotenvDefaultCaller vars = dotenvDomain "DEFAULT_API" vars "para a API de default"

dotenvFallbackCaller :: [(String, String)] -> IO Domain
dotenvFallbackCaller vars = dotenvDomain "FALLBACK_API" vars "para a API de fallback"

appSettings :: IO AppSettings
appSettings = do
  vars <- ENV.parseFile ".env"
  headServer <- dotenvHeadServer vars
  headCaller <- dotenvHeadCaller vars
  helper1Caller <- dotenvHelper1Caller vars
  defaultCaller <- dotenvDefaultCaller vars
  fallbackCaller <- dotenvFallbackCaller vars
  return $ AppSettings
    { APPS.headServer = headServer
    , APPS.headDomain = headCaller
    , APPS.helper1Domain = helper1Caller
    , APPS.defaultApiDomain = defaultCaller
    , APPS.fallbackApiDomain = fallbackCaller
    }

main :: IO ()
main = do
  connstr <- dotenvConnstr
  exposedport <- dotenvExposedport

  appSettings' <- appSettings

  pool <- initConnectionPool connstr
  mvar <- newEmptyMVar
  manager' <- liftIO (NETWORK.newManager NETWORK.defaultManagerSettings)
  migrateDB (APPS.headServer appSettings') connstr
  run exposedport (serve proxyServidor $ pagamentoServidor pool mvar manager' appSettings')
