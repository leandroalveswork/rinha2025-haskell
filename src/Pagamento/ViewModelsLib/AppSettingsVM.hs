module Pagamento.ViewModelsLib.AppSettingsVM
  ( AppSettings(AppSettings)
  , headServer
  , headDomain
  , helper1Domain
  , defaultApiDomain
  , fallbackApiDomain
  , Domain(Domain)
  , dmHostname 
  , dmPort
  , selfServerId
  ) where

data Domain = Domain
  { dmHostname :: String
  , dmPort :: Int 
  }

data AppSettings = AppSettings
  { headServer :: Bool
  , headDomain :: Domain
  , helper1Domain :: Domain
  , defaultApiDomain :: Domain
  , fallbackApiDomain :: Domain
  } 

selfServerId :: AppSettings -> Int
selfServerId appSettings = if headServer appSettings then 0 else 1
