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
import Pagamento.ViewModelsLib.Processor (processorCode, Processor)

type ProcessorApi = "payments" :> ReqBody '[JSON] PaymentSync 
    :> Post '[JSON] AnyMessage
  :<|> "payments" :> "service-health" 
    :> Get '[JSON] ServiceHealth

pagarPeloProcessor' :: PaymentSync -> SCLI.ClientM AnyMessage
obterSaude' :: SCLI.ClientM ServiceHealth

api :: Proxy ProcessorApi
api = Proxy

pagarPeloProcessor' :<|> obterSaude' = SCLI.client api

pagarPeloProcessor :: NETWORK.Manager -> Processor -> PaymentSync -> IO (Either SCLI.ClientError AnyMessage)
pagarPeloProcessor manager processor paymentSync = do
  SCLI.runClientM
    (pagarPeloProcessor' paymentSync) 
    (SCLI.mkClientEnv manager (SCLI.BaseUrl SCLI.Http ("payment-processor-" ++ processorCode processor) 8080 ""))

obterSaude :: NETWORK.Manager -> Processor -> IO (Either SCLI.ClientError ServiceHealth)
obterSaude manager processor = do
  SCLI.runClientM
    obterSaude'
    (SCLI.mkClientEnv manager (SCLI.BaseUrl SCLI.Http ("payment-processor-" ++ processorCode processor) 8080 ""))

