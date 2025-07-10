module Pagamento.ViewModelsLib.Processor 
  ( Processor(Default_, Fallback)
  , processorCode
  , processorId
  , getProcessorToSync
  ) where

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

--                                  allAmounts deve ficar ordenado de forma decrescente
getProcessorToSync :: Int -> Int -> [Int] -> Processor
getProcessorToSync previousRetries amount allAmounts 
  | previousRetries == 0 = Default_
  | previousRetries >= retriesBeforeFallback = Fallback
  | otherwise =
  let leastIndexRequired :: Int
      leastIndexRequired = (length allAmounts * previousRetries) `div` retriesBeforeFallback
      safeLeastIndex :: Int
      safeLeastIndex = min leastIndexRequired (length allAmounts)
  in if amount >= (allAmounts !! safeLeastIndex)
        then Fallback
        else Default_

