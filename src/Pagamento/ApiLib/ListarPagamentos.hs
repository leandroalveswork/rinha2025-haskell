{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Pagamento.ApiLib.ListarPagamentos
  ( listarPagamentos
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as SQL
import qualified Servant as S
import qualified Data.Time as TIME
import Pagamento.ViewModelsLib.PaymentsSummaryVM 
  ( PaymentsSummary(PaymentsSummary)
  , default_, fallback, zeroedSummary
  , PaymentSummary(PaymentSummary)
  , totalRequests, totalAmount
  )
import Pagamento.ViewModelsLib.Processor (processorId, Processor(Default_, Fallback))

listarPagamentos :: DP.Pool SQL.Connection -> TIME.UTCTime -> TIME.UTCTime -> S.Handler PaymentsSummary
listarPagamentos conns de ate = do
  sumarios <- liftIO $
    DP.withResource conns $ \conn ->
      (SQL.query conn
        (  "SELECT COUNT(PaymentId), SUM(Amount), ProcessorId"
        <> "  FROM PAYMENTS WHERE (CreateTimestamp BETWEEN ? AND ?)"
        <> "    AND Finished = 1"
        <> "  GROUP BY TunnelId;"
        )
        (de, ate)
        :: IO [(Int, Rational, Int)])

  (let sumarioDefault :: PaymentSummary
       sumarioDefault = DM.fromMaybe zeroedSummary 
         $ fmap (\(reqs, amount, _) -> PaymentSummary { totalRequests = reqs, totalAmount = amount })
         $ DL.find (\(_, _, pId) -> pId == processorId Default_) sumarios
       sumarioFallback :: PaymentSummary
       sumarioFallback = DM.fromMaybe zeroedSummary 
         $ fmap (\(reqs, amount, _) -> PaymentSummary { totalRequests = reqs, totalAmount = amount })
         $ DL.find (\(_, _, pId) -> pId == processorId Fallback) sumarios

   in return $ PaymentsSummary { fallback = sumarioFallback, default_ = sumarioDefault } )

