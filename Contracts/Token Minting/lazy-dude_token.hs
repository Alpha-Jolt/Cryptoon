{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

import PlutusTx.Prelude
import Ledger
import Ledger.Value
import Plutus.Contract
import Plutus.Trace.Emulator

-- Token Symbol Constructor
lazyDudeToken :: CurrencySymbol
lazyDudeToken = CurrencySymbol "Lazy Dude"

-- Watch Time Logic within the contract
watchTime :: Integer -> Integer -> Integer -> [Integer] -> Double
watchTime totalDuration introDuration postCreditDuration skippedParts = do
    let totalWatchTime = fromIntegral totalDuration - (fromIntegral introDuration + fromIntegral postCreditDuration)
    let excludeSkippedParts [] watchTime' = watchTime'
        excludeSkippedParts (part:rest) watchTime' =
            if fromIntegral introDuration < fromIntegral part && fromIntegral part < fromIntegral totalDuration
                then excludeSkippedParts rest (watchTime' - (fromIntegral part - fromIntegral introDuration))
                else excludeSkippedParts rest watchTime'
    excludeSkippedParts skippedParts totalWatchTime

-- Custom Policy for Token Minting
customTokenPolicy :: AssetClass -> (Integer, Integer, Integer, [Integer]) -> ScriptContext -> Bool
customTokenPolicy ac (totalDuration, introDuration, postCreditDuration, skippedParts) ctx =
    let watchHours = watchTime totalDuration introDuration postCreditDuration skippedParts
        tokens = watchHours
        tokenName = TokenName "Lazy Dude"
        assetClass = AssetClass (lazyDudeToken, tokenName)
    in assetClassValueOf (valuePaidTo (scriptContextTxInfo ctx) ownPubKey) ac == tokens

-- The contract that mints tokens based on the custom policy
mintTokensContract :: Contract (Integer, Integer, Integer, [Integer]) BlockchainActions () ()
mintTokensContract = do
    logInfo "Waiting for token minting trigger..."
    selectList [mintTokens]

-- The endpoint to trigger token minting
mintTokens :: Promise (Integer, Integer, Integer, [Integer]) MintingSchema Text ()
mintTokens = endpoint @"mint-tokens" $ \(totalDuration, introDuration, postCreditDuration, skippedParts) -> do
    logInfo "Minting tokens..."
    ownPK <- ownPubKey
    let watchHours = watchTime totalDuration introDuration postCreditDuration skippedParts
        value = Value.singleton lazyDudeToken (TokenName "Lazy Dude") watchHours
        customPolicy = customTokenPolicy (AssetClass (lazyDudeToken, TokenName "Lazy Dude")) (totalDuration, introDuration, postCreditDuration, skippedParts)
    tx <- submitTxConstraintsWith @Void ownPK (mustMintValue value <> mustValidateIn customPolicy)
    logInfo $ "Tokens minted: " ++ show tx
    void $ awaitTxConfirmed $ txId tx

-- The main function to run the contract in the emulator
main :: IO ()
main = runEmulatorTraceIO $ do
    h <- activateContractWallet (Wallet 1) mintTokensContract
    callEndpoint @"mint-tokens" h (3600, 120, 180, [600, 900, 250])

-- Token Minting Contract
-- YOLO - Web3 Streaming
