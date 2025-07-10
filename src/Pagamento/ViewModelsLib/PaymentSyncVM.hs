{-# LANGUAGE DeriveGeneric #-}

module Pagamento.ViewModelsLib.PaymentSyncVM
  ( PaymentSync(PaymentSync)
  , correlationId
  , amount
  , requestedAt
  , fromPaymentVM
  ) where

import qualified Data.Time as TIME
import Data.Aeson
import GHC.Generics

import qualified Pagamento.ViewModelsLib.PaymentVM as PAYVM

data PaymentSync = PaymentSync
  { correlationId :: String
  , amount :: Rational
  , requestedAt :: TIME.UTCTime
  } deriving Generic

instance ToJSON PaymentSync

fromPaymentVM :: PAYVM.Payment -> TIME.UTCTime -> PaymentSync
fromPaymentVM (PAYVM.Payment { PAYVM.correlationId = correlatId, PAYVM.amount = amount' }) time =
  PaymentSync { correlationId = correlatId, amount = amount', requestedAt = time }
