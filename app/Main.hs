module Main where

import Data.Maybe                  (fromMaybe)
import Text.Read                   (readMaybe)
import Network.Wai.Handler.Warp
import Servant
--import qualified Data.Pool as DP
import qualified Configuration.Dotenv as ENV
import Pagamento.ApiLib.Api        (PagamentoApi, pagamentoServidor)
import RepositoryLib.Repository    (migrateDB, initConnectionPool, dotenvConnstr)

proxyServidor :: Proxy PagamentoApi
proxyServidor = Proxy

dotenvExposedport :: IO Int
dotenvExposedport = do
  vars <- ENV.parseFile ".env"
  let port = fromMaybe "80" (lookup "RINHAFX_PORT" vars)
  let validatedPort = readMaybe port
  case validatedPort of
    Nothing        -> fail "Falha ao ler porta configurada"
    Just validPort -> return validPort

main :: IO ()
main = do
  connstr <- dotenvConnstr
  exposedport <- dotenvExposedport
  pool <- initConnectionPool connstr
  migrateDB connstr
  run exposedport (serve proxyServidor $ pagamentoServidor pool)
