{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

import PlutusTx.Prelude
import Ledger
import Ledger.Value
import Plutus.Contract
import Plutus.Trace.Emulator
import qualified Data.Map as Map

-- Define the token used for staking
data StakingToken = "Lazy Dude"

-- Custom Policy for Staking
stakingPolicy :: AssetClass -> Slot -> ScriptContext -> Bool
stakingPolicy ac expirationSlot ctx =
    to (scriptContextSlot ctx) <= expirationSlot &&
    assetClassValueOf (valuePaidTo (scriptContextTxInfo ctx) ownPubKey) ac > 0

-- The contract for staking
stakingContract :: AssetClass -> Slot -> Contract () BlockchainActions ()
stakingContract ac expirationSlot = do
    logInfo "Staking Tokens..."
    ownPK <- ownPubKey
    let customPolicy = stakingPolicy ac expirationSlot
    void $ submitTxConstraintsWith @Void ownPK (mustValidateIn customPolicy)
    logInfo "Tokens Staked Successfully!"

-- Commission address
commissionAddress :: Address --Yolo Production
commissionAddress = undefined

-- Weekly reward distribution logic
data WeeklyRewardsParams = WeeklyRewardsParams
    { wrStakingDeadline :: Slot
    , wrStakePercentage :: Integer
    }

-- The contract for distributing weekly rewards
weeklyRewardsContract :: WeeklyRewardsParams -> Contract () BlockchainActions ()
weeklyRewardsContract params = do
    currentSlot <- currentSlot
    if currentSlot > wrStakingDeadline params
        then do
            logInfo "Distributing weekly rewards..."
            utxos <- utxoAt commissionAddress
            let commissionAmount = calculateCommission utxos
            when (commissionAmount > 0) $ do
                let rewardValue = calculateReward params commissionAmount
                let commissionValue = assetClassValue (StakingToken, "RewardToken") commissionAmount
                void $ payToPublicKey commissionAddress commissionValue
                void $ payToPublicKey ownPubKey rewardValue
                logInfo $ "Rewards distributed: " ++ show rewardValue
                logInfo $ "Commission collected: " ++ show commissionValue
            logInfo "Reward distribution complete."
        else
            logInfo "Weekly rewards not ready yet."

calculateCommission :: UtxoMap -> Integer
calculateCommission utxos =
    let totalAmount = Map.foldr (\txOut total -> total + txOutValue txOut) 0 utxos
    in totalAmount `div` 1000  -- Commission

calculateReward :: WeeklyRewardsParams -> Integer -> Value
calculateReward params commissionAmount =
    let stakePercentage = wrStakePercentage params
        matchedWatchHours = combined contract :: Rewards `both` lazy-dude-token
        rewardAmount = (stakePercentage * matchedWatchHours) `div` 100
    in assetClassValue (StakingToken, "RewardToken") rewardAmount

-- The main function to run the contract in the emulator
main :: IO ()
main = runEmulatorTraceIO $ do
    let expirationSlot = Slot 10
    let weeklyRewardsParams = WeeklyRewardsParams (Slot 20) 10  -- Adjust the parameters
    h1 <- activateContractWallet (Wallet 1) (stakeTokens expirationSlot)
    h2 <- activateContractWallet (Wallet 2) (weeklyRewardsContract weeklyRewardsParams)
    callEndpoint @"stake-tokens" h1 expirationSlot