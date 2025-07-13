{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Pagamento.CallerLib.Caller (pagarPeloProcessor, obterSaude) where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import qualified Network.HTTP.Client as NETWORK 
import Servant.API
import Servant.Client
import Pagamento.ViewModelsLib.AnyMessageVM (AnyMessage)
import Pagamento.ViewModelsLib.PaymentSyncVM (PaymentSync)
import Pagamento.ViewModelsLib.ServiceHealthVM (ServiceHealth)
import Pagamento.ViewModelsLib.Processor (processorCode, Processor)

type ProcessorApi = "payments" :> ReqBody '[JSON] PaymentSync 
    :> Post '[JSON] AnyMessage
  :<|> "payments" :> "service-health" 
    :> Get '[JSON] ServiceHealth

pagarPeloProcessor' :: PaymentSync -> ClientM AnyMessage
obterSaude' :: ClientM ServiceHealth

api :: Proxy ProcessorApi
api = Proxy

pagarPeloProcessor' :<|> obterSaude' = client api

pagarPeloProcessor :: NETWORK.Manager -> Processor -> PaymentSync -> IO (Either ClientError AnyMessage)
pagarPeloProcessor manager processor paymentSync = do
  runClientM
    (pagarPeloProcessor' paymentSync) 
    (mkClientEnv manager (BaseUrl Http (processorCode processor ++ "-processor-default") 8080 ""))

obterSaude :: NETWORK.Manager -> Processor -> IO (Either ClientError ServiceHealth)
obterSaude manager processor = do
  runClientM
    obterSaude'
    (mkClientEnv manager (BaseUrl Http (processorCode processor ++ "-processor-default") 8080 ""))

