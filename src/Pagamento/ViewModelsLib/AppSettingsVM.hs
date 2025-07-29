module Pagamento.ViewModelsLib.AppSettingsVM
  ( AppSettings(AppSettings)
  , headServer
  , defaultHostname
  , defaultPort
  , fallbackHostname
  , fallbackPort
  ) where

data AppSettings = AppSettings
  { headServer :: Bool
  , defaultHostname :: String
  , defaultPort :: Int
  , fallbackHostname :: String
  , fallbackPort :: Int
  } 
