{-# LANGUAGE DeriveGeneric #-}

module Pagamento.ViewModelsLib.AnyMessageVM
  ( AnyMessage(AnyMessage)
  , message
  ) where

import Data.Aeson
import GHC.Generics

data AnyMessage = AnyMessage
  { message :: String
  } deriving Generic

instance FromJSON AnyMessage

