{-# LANGUAGE DeriveGeneric #-}

module Pagamento.ViewModelsLib.PaymentVM
  ( Payment(Payment)
  , correlationId
  , amount
  ) where

import Data.Aeson
import GHC.Generics
import Data.Scientific

data Payment = Payment
  { correlationId :: String
  , amount :: Scientific
  } deriving Generic

instance ToJSON Payment
instance FromJSON Payment

