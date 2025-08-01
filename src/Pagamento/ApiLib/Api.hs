{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Pagamento.ApiLib.Api
  ( pagamentoServidor
  ) where

import Control.Concurrent (MVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Time as TIME
import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as SQL
import qualified Network.HTTP.Client as NETWORK
import Servant
import qualified Servant as S
import Pagamento.ViewModelsLib.PaymentVM (Payment)
import Pagamento.ViewModelsLib.PaymentSyncVM (PaymentSync)
import Pagamento.ViewModelsLib.PaymentsSummaryVM (PaymentsSummary)
import Pagamento.ApiLib.ApiType (PagamentoApi)
import Pagamento.ApiLib.Pagar (pagar)
import Pagamento.ApiLib.ListarPagamentos (listarPagamentos) 
import Pagamento.ApiLib.ReceberTrabalho (receberTrabalho, receberTrabalho_) 
import Pagamento.ApiLib.RevisarAgendamentos (revisarAgendamentosGenerico) 
import Pagamento.ViewModelsLib.AppSettingsVM (AppSettings (headServer))

revisarAgendamentos :: DP.Pool SQL.Connection -> MVar () -> NETWORK.Manager -> AppSettings -> S.Handler S.NoContent
revisarAgendamentos conns mvar manager appSettings
  | headServer appSettings = liftIO (revisarAgendamentos_ conns mvar manager appSettings)
    >> return S.NoContent
  | otherwise = S.throwError S.err404

revisarAgendamentos_ :: DP.Pool SQL.Connection -> MVar () -> NETWORK.Manager -> AppSettings -> IO ()
revisarAgendamentos_ conns mvar manager appSettings = (revisarAgendamentosGenerico receberTrabalho_) conns mvar manager appSettings

pagamentoServidor :: DP.Pool SQL.Connection -> MVar () -> NETWORK.Manager -> AppSettings -> Server PagamentoApi
pagamentoServidor conns mvar manager appSettings = pagar conns mvar manager appSettings
  :<|> listarPagamentos conns
  :<|> receberTrabalho conns mvar manager appSettings
  :<|> revisarAgendamentos conns mvar manager appSettings

