{-# LANGUAGE OverloadedStrings #-}

module Pagamento.ApiLib.RevisarAgendamentos
  ( revisarAgendamentosGenerico
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, MVar, takeMVar, tryPutMVar)
import Control.Exception (bracket)
import qualified Data.List as DL
import qualified Data.Time as TIME
import qualified Network.HTTP.Client as NETWORK 
import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as SQL
import Pagamento.ViewModelsLib.AppSettingsVM (AppSettings)
import Pagamento.ViewModelsLib.PlanVM (balanceUnprocessed, fromDatabase, planId, planServerId, planVersion, Plan)
import qualified Pagamento.InternalCallerLib.InternalCaller as INCALL
import Control.Monad (forM_)
import Data.String (IsString(fromString))

revisarAgendamentosGenerico :: (DP.Pool SQL.Connection -> MVar () -> NETWORK.Manager -> AppSettings -> Plan -> IO ()) 
  -> DP.Pool SQL.Connection -> MVar () -> NETWORK.Manager -> AppSettings -> IO ()
revisarAgendamentosGenerico receberTrabalho_ conns mvar manager appSettings = do 
  free <- tryPutMVar mvar ()
  if free
     then bracket (return mvar) (\var' -> takeMVar var') (\var' -> do
        planos <- fmap (map fromDatabase) $ liftIO $
          DP.withResource conns $ \conn ->
            (SQL.query_ conn
              (  "SELECT PL.PlanId, PA.PaymentId, PL.PlanServerId, PlanVersion, FireTimestamp FROM PAYMENT_PLANS PL"
              <> "  INNER JOIN PAYMENTS PA ON PA.PaymentId = PL.PaymentId AND PA.ProcessorId IS NULL"
              <> "  ORDER BY PL.PaymentId;"
              )
            ):: IO [(Int, Int, Int, Int, TIME.UTCTime)]
        let planosComparativo = filter (uncurry (/=)) $ zip planos (balanceUnprocessed planos)
        let planosNovos = map snd planosComparativo
        if not (null planosNovos)
           then do
              _ <- DP.withResource conns $ \conn ->
                  (SQL.execute conn
                    (  "UPDATE PAYMENT_PLANS SET "
                    <> "  PlanVersion = PlanVersion + 1, PlanServerId = 1 - PlanServerId"
                    <> "  WHERE PlanId IN ?;")
                    (SQL.Only $ SQL.In (map planId planosNovos))
                  )
              forM_ 
                planosNovos
                (\plano -> do
                  _ <- forkIO $ (
                      (if planServerId plano == 1
                          then do
                            _ <- INCALL.receberTrabalho manager appSettings plano
                            return ()
                          else do 
                            _ <- receberTrabalho_ conns var' manager appSettings plano
                            return ()
                      )
                    )
                  return ()
                )
           else return ()
      )
     else return ()

