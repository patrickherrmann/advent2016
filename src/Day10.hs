{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Day10 where

import Protolude hiding (try)
import Prelude (read)
import Control.Lens
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Text
import Data.MultiMap (MultiMap)
import Data.Map (Map)
import qualified Data.MultiMap as MMap
import qualified Data.Map as Map
import Data.Vector (Vector)

type BotId = Int
type OutputBinId = Int
type ChipId = Int

data BotsState = BotsState
  { _bots :: MultiMap BotId ChipId
  , _bins :: MultiMap OutputBinId ChipId
  , _history :: Vector (BotId, ChipId, ChipId)
  , _policies :: Map BotId (ChipRecipient, ChipRecipient)
  }

data BotInstruction
  = ReceiveChip ChipId BotId
  | SplitChips BotId ChipRecipient ChipRecipient
  deriving (Show)

data ChipRecipient
  = OutputBin OutputBinId
  | Bot BotId
  deriving (Show)

makeLenses ''BotsState

runBotSimulation :: [BotInstruction] -> BotsState
runBotSimulation is = execState simulate s
  where
    s = BotsState MMap.empty MMap.empty [] Map.empty
    simulate = do
      setupState is
      enactPolicies

enactPolicies :: State BotsState ()
enactPolicies = do
    botIds <- uses policies Map.keys
    changed <- any identity <$> traverse enactBot botIds
    when changed enactPolicies
  where
    enactBot botId = uses bots (MMap.lookup botId) >>= \case
      (a:b:_) -> do
        let (lo:hi:_) = sort [a, b]
        bots %= MMap.delete botId
        Just (recLo, recHi) <- uses policies (Map.lookup botId)
        giveChip lo recLo
        giveChip hi recHi
        history <>= [(botId, lo, hi)]
        return True
      _ -> return False

setupState :: [BotInstruction] -> State BotsState ()
setupState = traverse_ followBotInstruction
  where
    followBotInstruction = \case
      ReceiveChip chipId botId -> bots %= MMap.insert botId chipId
      SplitChips botId recLo recHi -> policies %= Map.insert botId (recLo, recHi)

giveChip :: ChipId -> ChipRecipient -> State BotsState ()
giveChip c = \case
  OutputBin binId -> bins %= MMap.insert binId c
  Bot botId -> bots %= MMap.insert botId c

parseDay10 :: Text -> [BotInstruction]
parseDay10 t = is
  where
    Right is = parse botInstructionsP "" t
    botInstructionsP :: Parser [BotInstruction]
    botInstructionsP = botInstructionP `sepBy` eol
    botInstructionP = receiveChipP <|> splitChipsP
    receiveChipP = do
      try $ string "value "
      chipId <- numP
      string " goes to bot "
      botId <- numP
      return $ ReceiveChip chipId botId
    splitChipsP = do
      string "bot "
      botId <- numP
      string " gives low to "
      recLo <- chipRecipientP
      string " and high to "
      recHi <- chipRecipientP
      return $ SplitChips botId recLo recHi
    chipRecipientP = outputBinP <|> botP
    outputBinP = do
      try $ string "output "
      binId <- numP
      return $ OutputBin binId
    botP = do
      string "bot "
      botId <- numP
      return $ Bot botId
    numP = read <$> some digitChar
