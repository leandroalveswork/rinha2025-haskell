{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Pagamento.ApiLib.Pagar
  ( pagar
  ) where

import Data.Text (Text, pack)
import Data.Fixed (HasResolution (resolution))
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Time as TIME
import qualified Network.HTTP.Client as NETWORK 
import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as SQL
import qualified Servant as S
import qualified Pagamento.ViewModelsLib.PaymentVM as PAGVM
import Pagamento.ViewModelsLib.PaymentSyncVM (PaymentSync, fromPaymentVM, correlationId, amount, requestedAt)
import Pagamento.ViewModelsLib.Processor (Processor(Default_), processorId, getProcessorToSync, retentarIntervalo, safeAmountsArray)
import Pagamento.CallerLib.Caller (pagarPeloProcessor)
import Data.Scientific

pagar :: DP.Pool SQL.Connection -> NETWORK.Manager -> PAGVM.Payment -> S.Handler S.NoContent
pagar conns manager pagamento = do
  requestedAt' <- liftIO TIME.getCurrentTime
  liftIO $ putStrLn "Rinha> API Pagar"
  pagamentoSync <- return $ fromPaymentVM pagamento requestedAt'
  _ <- liftIO $
    DP.withResource conns $ \conn ->
      (SQL.execute conn
        "INSERT INTO AMOUNTS (Amount) VALUES (?);" 
        (SQL.Only (PAGVM.amount pagamento)))
  clientRes <- liftIO $ pagarPeloProcessor manager Default_ pagamentoSync
  _ <- (case clientRes of
             Left _ ->
               liftIO $ 
                 reagendarPagamento 
                   conns 
                   manager 
                   1 
                   (fromPaymentVM pagamento requestedAt')
             Right _ ->
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
  
reagendarPagamento :: DP.Pool SQL.Connection -> NETWORK.Manager -> Int -> PaymentSync -> IO ()
reagendarPagamento conns manager retries pagamento = do
  _ <- forkIO $ (
    do
      threadDelay (
        1000000 * (fromIntegral $ resolution $ 
          TIME.nominalDiffTimeToSeconds retentarIntervalo))
      amounts <- fmap safeAmountsArray $
        fmap (map (\(SQL.Only x) -> x)) $
          DP.withResource conns $ \conn ->
            ((SQL.query_ conn "SELECT Amount FROM AMOUNTS ORDER BY Amount DESC;") :: IO [SQL.Only Scientific])
      processor' <- return $ getProcessorToSync retries (amount pagamento) amounts
      clientRes <- pagarPeloProcessor manager processor' pagamento
      _ <- (case clientRes of
                 Left _ ->
                   reagendarPagamento 
                     conns
                     manager
                     (retries + 1)
                     pagamento
                 Right _ ->
                   salvarPagamento conns processor' retries pagamento
                   )
      return ()
    )
  return ()

