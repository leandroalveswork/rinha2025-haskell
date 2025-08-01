{-# LANGUAGE DeriveGeneric #-}

module Pagamento.ViewModelsLib.PlanVM
  ( Plan(Plan)
  , planId
  , paymentId
  , planServerId
  , planVersion
  , fireTimestamp
  , fromDatabase
  , balanceUnprocessed
  ) where

import qualified Data.Time as TIME
import Data.Aeson
import GHC.Generics
import Data.Scientific

import qualified Pagamento.ViewModelsLib.PaymentSyncVM as PAYVM
import Pagamento.ViewModelsLib.AppSettingsVM (AppSettings)

data Plan = Plan
  { planId :: Int
  , paymentId :: Int
  , planServerId :: Int
  , planVersion :: Int
  , fireTimestamp :: TIME.UTCTime
  } deriving (Generic, Eq)

instance FromJSON Plan
instance ToJSON Plan

fromDatabase :: (Int, Int, Int, Int, TIME.UTCTime) -> Plan
fromDatabase (planId', payId, serverId, ver, fireTime) = Plan
  { planId = planId'
  , paymentId = payId
  , planServerId = serverId
  , planVersion = ver
  , fireTimestamp = fireTime
  }

balanceUnprocessed :: [Plan] -> [Plan]
balanceUnprocessed ps =
  let indexBound = length ps `div` 3
      changePlan :: Int -> Plan -> Plan
      changePlan nextId plan = plan { planServerId = nextId }
  in (map (changePlan 1) $ take indexBound ps) 
    ++ (map (changePlan 0) $ drop indexBound ps)
