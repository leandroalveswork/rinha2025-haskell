{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Pagamento.ApiLib.Pagar
  ( pagar
  ) where

import Data.Text (Text, pack)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import Data.Scientific
import qualified Data.Time as TIME
import qualified Network.HTTP.Client as NETWORK 
import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as SQL
import qualified Servant as S
import qualified Pagamento.ViewModelsLib.PaymentVM as PAGVM
import Pagamento.ViewModelsLib.PaymentSyncVM (PaymentSync, fromPaymentVM, correlationId, amount, requestedAt)
import Pagamento.ViewModelsLib.Processor (Processor(Default_), processorId, getProcessorToSync, retentarIntervalo, safeAmountsArray)
import Pagamento.CallerLib.Caller (pagarPeloProcessor)
import Pagamento.ViewModelsLib.AppSettingsVM (AppSettings)

pagar :: DP.Pool SQL.Connection -> NETWORK.Manager -> AppSettings -> PAGVM.Payment -> S.Handler S.NoContent
pagar conns manager appSettings pagamento = do
  requestedAt' <- liftIO TIME.getCurrentTime
  let pagamentoSync = fromPaymentVM pagamento requestedAt'
  _ <- liftIO $
    DP.withResource conns $ \conn ->
      (SQL.execute conn
        "INSERT INTO AMOUNTS (Amount) VALUES (?);" 
        (SQL.Only (PAGVM.amount pagamento)))
  clientRes <- liftIO $ pagarPeloProcessor manager appSettings Default_ pagamentoSync
  _ <- (case clientRes of
             Left _ -> do
               liftIO $ 
                 reagendarPagamento 
                   conns 
                   manager 
                   appSettings 
                   1 
                   (fromPaymentVM pagamento requestedAt')
             Right _ -> do
               liftIO $ salvarPagamento conns Default_ 0 pagamentoSync)
  return S.NoContent

salvarPagamento :: DP.Pool SQL.Connection -> Processor -> Int -> PaymentSync -> IO ()
salvarPagamento conns proc retriesCount pagamento = do
  _ <- DP.withResource conns $ \conn ->
         (SQL.execute conn
           ( "INSERT INTO PAYMENTS (Code, Amount, CreateTimestamp, NextRetryTimestamp, Retries, Finished, ProcessorId)"
             <> "  VALUES (?, ?, ?, NULL, ?, 1, ?);"
             )
           ( pack (correlationId pagamento) :: Text
             , amount pagamento :: Scientific
             , (requestedAt pagamento) :: TIME.UTCTime
             , retriesCount :: Int
             , (processorId proc) :: Int
             ))
  return ()
  
reagendarPagamento :: DP.Pool SQL.Connection -> NETWORK.Manager -> AppSettings -> Int -> PaymentSync -> IO ()
reagendarPagamento conns manager appSettings retries pagamento = do
  _ <- forkIO $ (
    do
      threadDelay ((floor . (1000000 *) . toRational) retentarIntervalo)
      amounts <- fmap safeAmountsArray $
        fmap (map (\(SQL.Only x) -> x)) $
          DP.withResource conns $ \conn ->
            ((SQL.query_ conn "SELECT Amount FROM AMOUNTS ORDER BY Amount DESC;") :: IO [SQL.Only Scientific])
      processor' <- return $ getProcessorToSync retries (amount pagamento) amounts
      clientRes <- pagarPeloProcessor manager appSettings processor' pagamento
      _ <- (case clientRes of
                 Left _ -> do
                   reagendarPagamento 
                     conns
                     manager
                     appSettings
                     (retries + 1)
                     pagamento
                 Right _ -> do
                   salvarPagamento conns processor' retries pagamento
                   )
      return ()
    )
  return ()

