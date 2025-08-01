{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Scientific

data PaymentSummary = PaymentSummary
  { totalRequests :: Int
  , totalAmount :: Scientific
  } deriving Generic

instance ToJSON PaymentSummary
instance FromJSON PaymentSummary

zeroedSummary :: PaymentSummary
zeroedSummary = PaymentSummary { totalAmount = 0, totalRequests = 0 } 

data PaymentsSummary = PaymentsSummary
  { default_ :: PaymentSummary
  , fallback :: PaymentSummary
  } deriving Generic

instance ToJSON PaymentsSummary where
  toJSON (PaymentsSummary d c) =
    object [(fromString "default") .= d, (fromString "fallback") .= c]

instance FromJSON PaymentsSummary where
  parseJSON (Object v) = PaymentsSummary <$> v .: "default" <*> v .: "fallback" 
