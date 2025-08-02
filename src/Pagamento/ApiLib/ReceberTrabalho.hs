{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Pagamento.ApiLib.ReceberTrabalho
  ( receberTrabalho
  , receberTrabalho_
  ) where

import Data.Scientific (Scientific)
import qualified Data.List as DL
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay, MVar)
import qualified Data.Time as TIME
import qualified Network.HTTP.Client as NETWORK 
import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as SQL
import qualified Servant as S
import Pagamento.ViewModelsLib.PaymentSyncVM (PaymentSync (PaymentSync), correlationId, amount, requestedAt)
import Pagamento.ViewModelsLib.Processor (getProcessorToSync, retentarIntervalo, safeAmountsArray, processorId)
import Pagamento.ViewModelsLib.AppSettingsVM (AppSettings, headServer)
import Pagamento.CallerLib.Caller (pagarPeloProcessor)
import qualified Pagamento.InternalCallerLib.InternalCaller as INCALL
import Pagamento.RepositoryLib.FinalizarPagamento (finalizarPagamento)
import Pagamento.ApiLib.RevisarAgendamentos (revisarAgendamentosGenerico)
import qualified Pagamento.ViewModelsLib.PlanVM as PLAN
import RepositoryLib.Repository (pgLog)

receberTrabalho :: DP.Pool SQL.Connection -> MVar () -> NETWORK.Manager -> AppSettings -> PLAN.Plan -> S.Handler S.NoContent
receberTrabalho conns mvar manager appSettings plan
  | headServer appSettings = S.throwError S.err404 
  | otherwise = liftIO (receberTrabalho_ conns mvar manager appSettings plan)
    >> return S.NoContent

receberTrabalho_ :: DP.Pool SQL.Connection -> MVar () -> NETWORK.Manager -> AppSettings -> PLAN.Plan -> IO ()
receberTrabalho_ conns mvar manager appSettings (plan@PLAN.Plan 
  { PLAN.planId = planId
  , PLAN.fireTimestamp = fireTimestamp 
  , PLAN.planVersion = planVersion
  } ) = do
  _ <- forkIO $ (
    do
      now <- liftIO $ TIME.getCurrentTime
      let tDelay = (floor . (1000000 *) . (max 0) . toRational) (TIME.diffUTCTime fireTimestamp now) :: Int
      liftIO $ pgLog conns appSettings ("[RecebTrab] Delay de " ++ show tDelay ++ "micro seg") Nothing (Just planId) (Just planVersion) (Just now)
      threadDelay ((floor . (1000000 *) . (max 0) . toRational) (TIME.diffUTCTime fireTimestamp now))
      plans <- DP.withResource conns $ \conn ->
        (SQL.query conn
           (    "SELECT PA.PaymentId, PA.Code, PA.Amount, PA.CreateTimestamp, PA.Retries"
             <> "  FROM PAYMENT_PLANS PL"
             <> "    INNER JOIN PAYMENTS PA ON PA.PaymentId = PL.PaymentId AND PL.PlanVersion = ? AND PA.ProcessorId IS NULL"
             <> "    WHERE PlanId = ?;"
             )
           (planVersion, planId)
         :: IO [(Int, String, Scientific, TIME.UTCTime, Int)]
        )
      case plans of
           [] -> (do
              now2 <- liftIO $ TIME.getCurrentTime
              liftIO $ pgLog conns appSettings "[RecebTrab] O plano foi modificado" Nothing (Just planId) (Just planVersion) (Just now2)
              return ()
            )
           (dbRow:_) -> (do
              now3 <- liftIO $ TIME.getCurrentTime
              liftIO $ pgLog conns appSettings "[RecebTrab] O plano se manteve, tentando novamente" Nothing (Just planId) (Just planVersion) (Just now3)
              amounts <- fmap safeAmountsArray $
                fmap (map (\(SQL.Only x) -> x)) $
                  DP.withResource conns $ \conn ->
                    ((SQL.query_ conn "SELECT Amount FROM PAYMENTS WHERE ProcessorId IS NULL ORDER BY Amount;") :: IO [SQL.Only Scientific])
              let top25 = drop (max 0 (length amounts - 25)) amounts
              let less25 = take 25 amounts
              now4 <- liftIO $ TIME.getCurrentTime
              liftIO $ pgLog conns appSettings 
                ("[RecebTrab] Top 25 quantidades: " ++ (DL.intercalate ";" (map show top25) ))
                Nothing (Just planId) (Just planVersion) (Just now4)
              liftIO $ pgLog conns appSettings 
                ("[RecebTrab] 25 menores quantidades: " ++ (DL.intercalate ";" (map show less25) ))
                Nothing (Just planId) (Just planVersion) (Just now4)
              let (payId, correlat, amount', requestAt, retries) = dbRow 
              let processor' = getProcessorToSync retries amount' amounts
              let pagamento = PaymentSync { correlationId = correlat, amount = amount', requestedAt = requestAt }
              liftIO $ pgLog conns appSettings 
                ("[RecebTrab] ProcessorId: " ++ (show (processorId processor')) ++ ", retentativa n.: " ++ show retries)
                (Just correlat) (Just planId) (Just planVersion) (Just now4)
              clientRes <- pagarPeloProcessor manager appSettings processor' pagamento
              _ <- (case clientRes of
                       Left _ -> do
                         fireNow <- liftIO $ TIME.getCurrentTime
                         pgLog conns appSettings "[RecebTrab] Retentativa falhou" 
                            (Just correlat) (Just planId) (Just planVersion) (Just fireNow)
                         _ <- DP.withResource conns $ \conn ->
                            (SQL.execute conn
                               (  "UPDATE PAYMENT_PLANS SET FireTimestamp = ? WHERE PlanId = ?;"
                               <> "UPDATE PAYMENTS SET Retries = Retries + 1 WHERE PaymentId = ?;"
                               )
                               (TIME.addUTCTime retentarIntervalo fireNow, planId, payId)
                            )
                         pgLog conns appSettings "[RecebTrab] Update do FireTimestamp com sucesso" (Just correlat) (Just planId) (Just planVersion) (Just fireNow)
                         _ <- forkIO (chamarRevisarAgendamentos conns mvar manager appSettings)
                         let newPlan = plan { PLAN.fireTimestamp = (TIME.addUTCTime retentarIntervalo fireNow) }
                         receberTrabalho_ conns mvar manager appSettings newPlan
                       Right _ -> do
                         finalizarPagamento conns processor' payId
                         )
              return ())
    )
  return ()

chamarRevisarAgendamentos :: DP.Pool SQL.Connection -> MVar () -> NETWORK.Manager -> AppSettings -> IO ()
chamarRevisarAgendamentos conns mvar manager appSettings
  | headServer appSettings = revisarAgendamentosGenerico receberTrabalho_ conns mvar manager appSettings
  | otherwise = INCALL.revisarAgendamentos manager appSettings >> return ()

