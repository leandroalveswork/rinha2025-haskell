{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Pagamento.ApiLib.Pagar
  ( pagar, chamarRevisarAgendamentos
  ) where

import Data.Text (Text, pack)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (MVar, forkIO)
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
import qualified Pagamento.ViewModelsLib.PlanVM as PLAN

pagar :: DP.Pool SQL.Connection -> MVar () -> NETWORK.Manager -> AppSettings -> PAGVM.Payment -> S.Handler S.NoContent
pagar conns mvar manager appSettings pagamento = do
  requestedAt' <- liftIO TIME.getCurrentTime
  let pagamentoSync = fromPaymentVM pagamento requestedAt'
  paymentRows <- liftIO $
    DP.withResource conns $ \conn ->
      (SQL.query conn
           (    "WITH ROWS AS ("
             <> "  INSERT INTO PAYMENTS (Code, Amount, CreateTimestamp, Retries, ProcessorId)"
             <> "    VALUES (?, ?, ?, 1, NULL) RETURNING PaymentId"
             <> ")"
             <> "INSERT INTO PAYMENT_PLANS (PaymentId, PlanServerId, PlanVersion, FireTimestamp)"
             <> "  SELECT ROWS.PaymentId, ?, 1, ?"
             <> "  FROM ROWS"
             <> "  RETURNING PaymentId, PlanId;"
             )
           ( pack (correlationId pagamentoSync) :: Text
             , amount pagamentoSync :: Scientific
             , (requestedAt pagamentoSync) :: TIME.UTCTime
             , (selfServerId appSettings) :: Int
             , (TIME.addUTCTime retentarIntervalo requestedAt') :: TIME.UTCTime
             )) :: IO [(Int, Int)]
  let (payId, planId) = head paymentRows
  clientRes <- liftIO $ pagarPeloProcessor manager appSettings Default_ pagamentoSync
  _ <- (case clientRes of
             Left _ -> do
               liftIO $ (do
                 _ <- forkIO $ chamarRevisarAgendamentos conns mvar manager appSettings
                 let plan = (PLAN.Plan 
                                    { PLAN.planId = planId
                                    , PLAN.paymentId = payId
                                    , PLAN.planServerId = (selfServerId appSettings)
                                    , PLAN.planVersion = 1
                                    , PLAN.fireTimestamp = TIME.addUTCTime retentarIntervalo requestedAt'
                                    }
                            )
                  in receberTrabalho_ conns mvar manager appSettings plan
                 )
             Right _ -> do
               liftIO $ finalizarPagamento conns Default_ payId)
  return S.NoContent


chamarRevisarAgendamentos :: DP.Pool SQL.Connection -> MVar () -> NETWORK.Manager -> AppSettings -> IO ()
chamarRevisarAgendamentos conns mvar manager appSettings
  | headServer appSettings = revisarAgendamentosGenerico receberTrabalho_ conns mvar manager appSettings
  | otherwise = INCALL.revisarAgendamentos manager appSettings >> return ()

