module Pagamento.ViewModelsLib.Processor 
  ( Processor(Default_, Fallback)
  , processorCode
  , processorId
  , getProcessorToSync
  , retentarIntervalo
  , safeAmountsArray
  ) where

import Data.Scientific
import qualified Data.List as DL
import qualified Data.Time as TIME

data Processor = Default_ | Fallback
  deriving (Eq)

processorCode :: Processor -> String
processorCode Default_ = "default"
processorCode Fallback = "fallback"

processorId :: Processor -> Int
processorId Default_ = 1
processorId Fallback = 2

retriesBeforeFallback :: Int
retriesBeforeFallback = 6

--                                         allAmounts deve ficar ordenado de forma crescente
getProcessorToSync :: Int -> Scientific -> [Scientific] -> Processor
getProcessorToSync previousRetries amount allAmounts
  | previousRetries == 0 = Default_
  | previousRetries >= retriesBeforeFallback = Fallback
  | otherwise =
  let leastIndexRequired :: Int
      leastIndexRequired = (length allAmounts * (retriesBeforeFallback - previousRetries)) `div` retriesBeforeFallback
      safeLeastIndex :: Int
      safeLeastIndex = max leastIndexRequired 0
  in if amount >= (allAmounts !! safeLeastIndex)
        then Fallback
        else Default_

retentarIntervalo :: TIME.NominalDiffTime
retentarIntervalo = 15

safeAmountsArray :: [Scientific] -> [Scientific]
safeAmountsArray xs
  | length xs <= 6 = (reverse . DL.sort) ([1, 10, 100, 1000, 10000, 100000] ++ xs)
  | otherwise = xs

