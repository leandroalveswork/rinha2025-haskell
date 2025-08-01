{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Pagamento.ApiLib.Pagar
  ( pagar, chamarRevisarAgendamentos
  ) where

import Data.Text (Text, pack)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (MVar)
import Data.Scientific
import qualified Data.Time as TIME
import qualified Network.HTTP.Client as NETWORK 
import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as SQL
import qualified Servant as S
import qualified Pagamento.ViewModelsLib.PaymentVM as PAGVM
import Pagamento.ViewModelsLib.PaymentSyncVM (fromPaymentVM, correlationId, amount, requestedAt)
import Pagamento.ViewModelsLib.Processor (Processor(Default_), retentarIntervalo)
import Pagamento.ViewModelsLib.AppSettingsVM (AppSettings (headServer), selfServerId)
import Pagamento.CallerLib.Caller (pagarPeloProcessor)
import qualified Pagamento.InternalCallerLib.InternalCaller as INCALL
import Pagamento.RepositoryLib.FinalizarPagamento (finalizarPagamento)
import Pagamento.ApiLib.RevisarAgendamentos (revisarAgendamentosGenerico)
import Pagamento.ApiLib.ReceberTrabalho (receberTrabalho_)

pagar :: DP.Pool SQL.Connection -> MVar () -> NETWORK.Manager -> AppSettings -> PAGVM.Payment -> S.Handler S.NoContent
pagar conns mvar manager appSettings pagamento = do
  requestedAt' <- liftIO TIME.getCurrentTime
  let pagamentoSync = fromPaymentVM pagamento requestedAt'
  paymentRows <- liftIO $
    DP.withResource conns $ \conn ->
      (SQL.query conn
           (    "INSERT INTO PAYMENTS (Code, Amount, CreateTimestamp, Retries, ProcessorId)"
             <> "  VALUES (?, ?, ?, 1, NULL);"
             <> "INSERT INTO PAYMENT_PLANS (PaymentId, PlanServerId, PlanVersion, FireTimestamp) VALUES (LASTVAL(), ?, 1, ?);"
             <> "  RETURNING CURRVAL('PAYMENTS', 'PaymentId'), PlanId;"
             )
           ( pack (correlationId pagamentoSync) :: Text
             , amount pagamentoSync :: Scientific
             , (requestedAt pagamentoSync) :: TIME.UTCTime
             , (1 - selfServerId appSettings) :: Int
             , (TIME.addUTCTime retentarIntervalo requestedAt') :: TIME.UTCTime
             )) :: IO [(Int, Int)]
  let (payId, planId) = head paymentRows
  clientRes <- liftIO $ pagarPeloProcessor manager appSettings Default_ pagamentoSync
  _ <- (case clientRes of
             Left _ -> do
               liftIO $
                 chamarRevisarAgendamentos conns mvar manager appSettings
             Right _ -> do
               liftIO $ finalizarPagamento conns Default_ payId)
  return S.NoContent


chamarRevisarAgendamentos :: DP.Pool SQL.Connection -> MVar () -> NETWORK.Manager -> AppSettings -> IO ()
chamarRevisarAgendamentos conns mvar manager appSettings
  | headServer appSettings = revisarAgendamentosGenerico receberTrabalho_ conns mvar manager appSettings
  | otherwise = INCALL.revisarAgendamentos manager appSettings >> return ()

