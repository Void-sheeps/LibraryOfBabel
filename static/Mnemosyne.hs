
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Text as T
import Data.Maybe (fromMaybe)

-- Base data structures
data Mnemosyne = Mnemosyne
  { state     :: State
  , shortTerm :: ShortTermMemory
  , longTerm  :: LongTermMemory
  , realm     :: Realm
  } deriving (Show)

data State = State Int deriving (Show)
type ShortTermMemory = [T.Text]
type LongTermMemory = [T.Text]
type Realm = T.Text
type Input = T.Text

-- Stub functions for the cycle
nextState :: State -> Input -> State
nextState (State s) _ = State (s + 1)

interLegere :: Input -> State -> Realm -> Maybe T.Text
interLegere input (State s) realm = Just $ T.pack $ "Processed '" ++ T.unpack input ++ "' at state " ++ show s ++ " in realm '" ++ T.unpack realm ++ "'"

appendShortTerm :: T.Text -> ShortTermMemory -> ShortTermMemory
appendShortTerm = (:)

recordLongTerm :: T.Text -> LongTermMemory -> LongTermMemory
recordLongTerm = (:)

-- The provided function
cycleMnemosyne :: Input -> Mnemosyne -> Mnemosyne
cycleMnemosyne input sys@Mnemosyne{..} =
  let newState   = nextState state input
      processed  = interLegere input newState realm

      newShort   = fromMaybe shortTerm $ fmap (`appendShortTerm` shortTerm) processed
      newLong    = fromMaybe longTerm  $ fmap (`recordLongTerm`  longTerm)  processed

      stateLog   = T.pack $ "State Transition: " <> show newState
  in sys { state     = newState
         , shortTerm = appendShortTerm stateLog newShort
         , longTerm  = newLong
         }

-- Main function to run a simulation
main :: IO ()
main = do
  let initialSystem = Mnemosyne
        { state     = State 0
        , shortTerm = []
        , longTerm  = []
        , realm     = "Test Realm"
        }
      inputs = ["input1", "input2", "input3"]
      finalSystem = foldl (flip cycleMnemosyne) initialSystem inputs

  putStrLn "Initial System:"
  print initialSystem
  putStrLn "\nRunning simulation..."
  putStrLn "\nFinal System:"
  print finalSystem
  putStrLn "\nShort Term Memory Log:"
  mapM_ (putStrLn . T.unpack) (reverse $ shortTerm finalSystem)
