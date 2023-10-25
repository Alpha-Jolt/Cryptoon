-- Importing Modules
import PlutusTx.Prelude
import Ledger
import Ledger.Value
import Ledger.Ada

-- Define the NFT Datum, which can store any custom data related to the NFT.
data NFTDatum = NFTDatum
    { owner :: PubKeyHash
    , tokenURI :: TokenName
    } deriving Show

--NFT Contract
mintNFT :: TokenName -> Contract w s Text ()
mintNFT tn = do
    -- Get the caller's public key hash
    pkh <- pubKeyHash <$> ownPubKey

    -- Token Symbol Constructor
    lazyDudeToken :: CurrencySymbol
    lazyDudeToken = CurrencySymbol "Lazy Dude"

    -- Create a value representing the NFT using the TokenName
    let nftValue = Value.singleton (currencySymbol tn) tn 1

    -- Create the NFT Datum
    let nftDatum = NFTDatum pkh tn

    -- Mint the NFT
    void $ ledgerTxn <- forgeContract (singleton pkh nftValue) [nftDatum]

    -- Log a message indicating a successful mint
    logInfo @String $ "Minted NFT with TokenName: " ++ show tn

-- Define the Plutus endpoint to mint the NFT
endpoints :: Contract () NFTDatum Text ()
endpoints = do
    -- Define the "mint" endpoint, which takes a TokenName as a parameter
    Contract{..} <- endpoint @"mint" mintNFT

    -- Provide a response upon successful mint
    logInfo @String "NFT minted successfully"
    mintNFT (tokenURI nftDatum)

-- The main function to run the contract
main :: Contract () NFTDatum Text ()
main = endpoints

--Yolo Dynamic NFT Contract