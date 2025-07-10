{-# LANGUAGE DeriveGeneric #-}

module Pagamento.ViewModelsLib.ServiceHealthVM
  ( ServiceHealth(ServiceHealth)
  , failing
  , minResponseTime
  ) where

import Data.Aeson
import GHC.Generics

data ServiceHealth = ServiceHealth
  { failing :: Bool
  , minResponseTime :: Int
  } deriving Generic

instance FromJSON ServiceHealth
