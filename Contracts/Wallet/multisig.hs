{-# LANGUAGE OverloadedStrings #-}

import Cardano.Wallet.Primitive.Types
import Cardano.Wallet.Primitive.CoinSelection
import Cardano.Wallet.Primitive.Address
import Cardano.Wallet.Primitive.Coin
import Cardano.Wallet.Primitive.Types.Coin
import Cardano.Wallet.Primitive.Types.Tx
import Cardano.Wallet.Primitive.Mnemonic

-- Define a simple multisig wallet structure
data MultiSigWallet = MultiSigWallet
  { walletOwners :: [XPubKey]
  , walletRequiredSigners :: Int
  } deriving (Show)

-- Create a new multisig wallet
createMultiSigWallet :: [XPubKey] -> Int -> MultiSigWallet
createMultiSigWallet owners requiredSigners =
  MultiSigWallet owners requiredSigners

-- Sign a transaction with a multisig wallet
signTransaction :: MultiSigWallet -> Transaction -> [XPubKey] -> Either String Transaction
signTransaction wallet transaction signers
  | length signers /= walletRequiredSigners wallet = Left "Wrong number of signers"
  | not (all (`elem` walletOwners wallet) signers) = Left "Not all signers are part of the wallet"
  | otherwise = Right $ signTransactionWithKeys transaction signers


main :: IO ()
main = do
  -- Generate some test XPubKeys
  let owner1 = generateMnemonic (Entropy 15)
  let owner2 = generateMnemonic (Entropy 15)
  let owner3 = generateMnemonic (Entropy 15)

  let wallet = createMultiSigWallet [owner1, owner2, owner3] 2

  -- Create a transaction
  let inputs = [TxIn (TxId "inputTxId") 0]
  let outputs = [TxOut (Address "recipientAddress") (Coin 1000000)]
  let transaction = Tx inputs outputs

  -- Sign the transaction with two out of the three owners
  let signers = [owner1, owner2]
  case signTransaction wallet transaction signers of
    Left err -> putStrLn $ "Error: " ++ err
    Right signedTx -> putStrLn $ "Signed Transaction: " ++ show signedTx
