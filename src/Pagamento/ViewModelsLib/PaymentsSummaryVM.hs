{-# LANGUAGE DeriveGeneric #-}

module Pagamento.ViewModelsLib.PaymentsSummaryVM
  ( PaymentsSummary(PaymentsSummary)
  , PaymentSummary(PaymentSummary)
  , default_
  , fallback
  , zeroedSummary
  , totalRequests
  , totalAmount
  ) where

import Data.Aeson
import Data.Aeson.Key (fromString)
import GHC.Generics

data PaymentSummary = PaymentSummary
  { totalRequests :: Int
  , totalAmount :: Rational
  } deriving Generic

instance ToJSON PaymentSummary

zeroedSummary :: PaymentSummary
zeroedSummary = PaymentSummary { totalAmount = 0, totalRequests = 0 } 

data PaymentsSummary = PaymentsSummary
  { default_ :: PaymentSummary
  , fallback :: PaymentSummary
  } deriving Generic

instance ToJSON PaymentsSummary where
  toJSON (PaymentsSummary d c) =
    object [(fromString "default") .= d, (fromString "fallback") .= c]

