{-# LANGUAGE OverloadedStrings #-}

module Pagamento.RepositoryLib.FinalizarPagamento
  ( finalizarPagamento 
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as SQL
import Pagamento.ViewModelsLib.Processor (Processor, processorId)

finalizarPagamento :: DP.Pool SQL.Connection -> Processor -> Int ->  IO ()
finalizarPagamento conns processor payId = do
  _ <- liftIO $
    DP.withResource conns $ \conn ->
      (SQL.execute conn
           (  "UPDATE PAYMENTS SET ProcessorId = ? WHERE PaymentId = ?;"
           <> "DELETE FROM PAYMENT_PLANS WHERE PaymentId = ?;"
           )
           (processorId processor, payId, payId))
  return ()
