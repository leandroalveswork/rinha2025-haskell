{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Pagamento.InternalCallerLib.InternalCaller (receberTrabalho, revisarAgendamentos) where

import Data.Proxy
import qualified Data.Time as TIME
import qualified Network.HTTP.Client as NETWORK 
import qualified Servant.Client as SCLI
import qualified Servant as S
import Servant.API
import Pagamento.ViewModelsLib.AnyMessageVM (AnyMessage)
import Pagamento.ViewModelsLib.PaymentSyncVM (PaymentSync)
import Pagamento.ViewModelsLib.AppSettingsVM (AppSettings, headDomain, helper1Domain, dmHostname, dmPort)
import Pagamento.ViewModelsLib.Processor (Processor)
import Pagamento.ApiLib.ApiType (PagamentoApi)
import Pagamento.ViewModelsLib.PlanVM (Plan)

receberTrabalho' :: Plan -> SCLI.ClientM S.NoContent
revisarAgendamentos' :: SCLI.ClientM S.NoContent 

api :: Proxy PagamentoApi
api = Proxy

_ :<|> _ :<|> receberTrabalho' :<|> revisarAgendamentos' = SCLI.client api

receberTrabalho :: NETWORK.Manager -> AppSettings -> Plan -> IO (Either SCLI.ClientError ())
receberTrabalho manager appSettings plan = do
  let hostname = dmHostname (helper1Domain appSettings)
  let port = dmPort (helper1Domain appSettings)
  SCLI.runClientM
    (receberTrabalho' plan >> return ())
    (SCLI.mkClientEnv manager (SCLI.BaseUrl SCLI.Http hostname port ""))

revisarAgendamentos :: NETWORK.Manager -> AppSettings -> IO (Either SCLI.ClientError ())
revisarAgendamentos manager appSettings = do
  let hostname = dmHostname (headDomain appSettings)
  let port = dmPort (headDomain appSettings)
  SCLI.runClientM
    (revisarAgendamentos' >> return ())
    (SCLI.mkClientEnv manager (SCLI.BaseUrl SCLI.Http hostname port ""))
