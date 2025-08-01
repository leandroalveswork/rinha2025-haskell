{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Pagamento.ApiLib.ApiType
  ( PagamentoApi
  ) where

import qualified Data.Time as TIME
import Servant
import Pagamento.ViewModelsLib.PaymentVM (Payment)
import Pagamento.ViewModelsLib.PaymentsSummaryVM (PaymentsSummary)
import Pagamento.ViewModelsLib.PlanVM (Plan)

type PagamentoApi = "payments" :> ReqBody '[JSON] Payment
     :> PostNoContent
  :<|> "payments-summary" :> QueryParam "from" TIME.UTCTime :> QueryParam "to" TIME.UTCTime 
     :> Get '[JSON] PaymentsSummary
  :<|> "receber-trabalho" :> ReqBody '[JSON] Plan
     :> PostNoContent
  :<|> "revisar-agendamentos"
     :> PostNoContent
