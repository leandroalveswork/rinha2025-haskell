{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Pagamento.CallerLib.Caller (pagarPeloProcessor, obterSaude) where

import Data.Proxy
import qualified Network.HTTP.Client as NETWORK 
import qualified Servant.Client as SCLI
import Servant.API
import Pagamento.ViewModelsLib.AnyMessageVM (AnyMessage)
import Pagamento.ViewModelsLib.PaymentSyncVM (PaymentSync)
import Pagamento.ViewModelsLib.ServiceHealthVM (ServiceHealth)
import Pagamento.ViewModelsLib.Processor (processorCode, Processor(Default_))
import Pagamento.ViewModelsLib.AppSettingsVM (AppSettings, defaultHostname, defaultPort, fallbackHostname, fallbackPort)

type ProcessorApi = "payments" :> ReqBody '[JSON] PaymentSync 
    :> Post '[JSON] AnyMessage
  :<|> "payments" :> "service-health" 
    :> Get '[JSON] ServiceHealth

pagarPeloProcessor' :: PaymentSync -> SCLI.ClientM AnyMessage
obterSaude' :: SCLI.ClientM ServiceHealth

api :: Proxy ProcessorApi
api = Proxy

pagarPeloProcessor' :<|> obterSaude' = SCLI.client api

pagarPeloProcessor :: NETWORK.Manager -> AppSettings -> Processor -> PaymentSync -> IO (Either SCLI.ClientError AnyMessage)
pagarPeloProcessor manager appSettings processor paymentSync = do
  let hostname = if processor == Default_ then defaultHostname appSettings else fallbackHostname appSettings
  let port = if processor == Default_ then defaultPort appSettings else fallbackPort appSettings
  SCLI.runClientM
    (pagarPeloProcessor' paymentSync) 
    (SCLI.mkClientEnv manager (SCLI.BaseUrl SCLI.Http hostname port "payments"))

obterSaude :: NETWORK.Manager -> AppSettings -> Processor -> IO (Either SCLI.ClientError ServiceHealth)
obterSaude manager appSettings processor = do
  let hostname = if processor == Default_ then defaultHostname appSettings else fallbackHostname appSettings
  let port = if processor == Default_ then defaultPort appSettings else fallbackPort appSettings
  SCLI.runClientM
    obterSaude'
    (SCLI.mkClientEnv manager (SCLI.BaseUrl SCLI.Http hostname port "payments"))

