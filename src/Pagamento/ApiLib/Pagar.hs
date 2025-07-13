{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Pagamento.ApiLib.Pagar
  ( pagar
  ) where

import Data.Text (Text, pack)
import qualified Data.Time as TIME
import Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Client as NETWORK 
import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as SQL
import qualified Servant as S
import Pagamento.ViewModelsLib.PaymentVM (Payment, correlationId, amount)
import Pagamento.ViewModelsLib.PaymentSyncVM (fromPaymentVM)
import Pagamento.ViewModelsLib.Processor (Processor(Default_))
import Pagamento.CallerLib.Caller (pagarPeloProcessor)
import GHC.Float (fromRat)

pagar :: DP.Pool SQL.Connection -> NETWORK.Manager -> Payment -> S.Handler S.NoContent
pagar conns manager pagamento = do
  requestedAt <- liftIO TIME.getCurrentTime
  _ <- liftIO $ pagarPeloProcessor manager Default_ (fromPaymentVM pagamento requestedAt)
  -- TODO: Apenas inserir com Finished = 1 se der o codigo 200
  -- Se ficar sem ter processado, preencher next retry
  _ <- liftIO $
    DP.withResource conns $ \conn ->
      (SQL.execute conn
        (  "INSERT INTO PAYMENTS (Code, Amount, CreateTimestamp, NextRetryTimestamp, Retries, Finished, ProcessorId)"
          <> "  VALUES (?, ?, ?, NULL, 15), 0, 1, 1)"
          )
        ( pack (correlationId pagamento) :: Text
          , fromRat (amount pagamento) :: Double
          , requestedAt :: TIME.UTCTime
          ))
  return S.NoContent

