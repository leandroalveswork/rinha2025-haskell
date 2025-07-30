module Pagamento.ViewModelsLib.AppSettingsVM
  ( AppSettings(AppSettings)
  , headServer
  , helper1Domain
  , defaultApiDomain
  , fallbackApiDomain
  , Domain(Domain)
  , dmHostname 
  , dmPort
  ) where

data Domain = Domain
  { dmHostname :: String
  , dmPort :: Int 
  }

data AppSettings = AppSettings
  { headServer :: Bool
  , helper1Domain :: Domain
    , defaultApiDomain :: Domain
  , fallbackApiDomain :: Domain
  } 
