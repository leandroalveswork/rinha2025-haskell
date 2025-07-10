{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Pagamento.CallerLib.Caller (pagarPeloProcessor, obterSaude) where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
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

pagarPeloProcessor :: Processor -> PaymentSync -> IO (Either ClientError AnyMessage)
pagarPeloProcessor processor paymentSync = do
  manager' <- liftIO (newManager defaultManagerSettings)
  runClientM
    (pagarPeloProcessor' paymentSync) 
    (mkClientEnv manager' (BaseUrl Http (processorCode processor ++ "-processor-default") 8080 ""))

obterSaude :: Processor -> IO (Either ClientError ServiceHealth)
obterSaude processor = do
  manager' <- liftIO (newManager defaultManagerSettings)
  runClientM
    obterSaude'
    (mkClientEnv manager' (BaseUrl Http (processorCode processor ++ "-processor-default") 8080 ""))

