{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Pagamento.ApiLib.Api
  ( PagamentoApi
  , pagamentoServidor
  ) where

import qualified Data.Time as TIME
import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as SQL
import qualified Network.HTTP.Client as NETWORK
import Servant
import Pagamento.ViewModelsLib.PaymentVM (Payment)
import Pagamento.ViewModelsLib.PaymentSyncVM (PaymentSync)
import Pagamento.ViewModelsLib.PaymentsSummaryVM (PaymentsSummary)
import Pagamento.ApiLib.Pagar (pagar)
import Pagamento.ApiLib.ListarPagamentos (listarPagamentos) 
import Pagamento.ApiLib.ObterPagamentoPorId (obterPagamentoPorId) 

type PagamentoApi = "payments" :> ReqBody '[JSON] Payment
     :> PostNoContent
  :<|> "payments-summary" :> Capture "from" TIME.UTCTime :> Capture "to" TIME.UTCTime 
     :> Get '[JSON] PaymentsSummary
  :<|> "payments" :> Capture "id" Int
     :> Get '[JSON] PaymentSync

pagamentoServidor :: DP.Pool SQL.Connection -> NETWORK.Manager -> Server PagamentoApi
pagamentoServidor conns manager = pagar conns manager
  :<|> listarPagamentos conns
  :<|> obterPagamentoPorId conns

