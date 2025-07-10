{-# LANGUAGE DeriveGeneric #-}

module Pagamento.ViewModelsLib.PaymentVM
  ( Payment(Payment)
  , correlationId
  , amount
  ) where

import Data.Aeson
import GHC.Generics

data Payment = Payment
  { correlationId :: String
  , amount :: Rational
  } deriving Generic

instance ToJSON Payment
instance FromJSON Payment

