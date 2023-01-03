{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (ToSchema)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Prelude              (Semigroup (..), Show (..), String)
import qualified Prelude

data Game = Game
    { gChallenger      :: !PaymentPubKeyHash
    , gGuesser         :: !PaymentPubKeyHash
    , gStake           :: !Integer
    , gGuessDeadline   :: !POSIXTime
    , gProveDeadline   :: !POSIXTime
    , gNFT             :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Game

data GameChoice = A | B | C | D
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq)
 
instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    A == A = True
    B == B = True
    C == C = True
    D == D = True
    _ == _ = False

PlutusTx.unstableMakeIsData ''GameChoice

data GameDatum = GameDatum BuiltinByteString [Maybe GameChoice, Maybe GameChoice]
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mcs == GameDatum bs' mcs' = (bs == bs') && (mcs == mcs')

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer =   Guess [GameChoice, GameChoice] 
                    | Prove BuiltinByteString GameChoice
                    | ClaimChallenger 
                    | ClaimGuesser
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-}
gameDatum :: Maybe Datum -> Maybe GameDatum
gameDatum md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d


bsA, bsB, bsC, bsD :: BuiltinByteString
bsA = "A"
bsB = "B"
bsC = "C"
bsD = "D"

unsafeFromGameChoice :: GameChoice -> BuiltinByteString
unsafeFromGameChoice A = bsA
unsafeFromGameChoice B = bsB
unsafeFromGameChoice C = bsC
unsafeFromGameChoice D = bsD 


{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> GameDatum -> GameRedeemer -> ScriptContext -> Bool 
mkGameValidator game dat red ctx = 
    traceIfFalse "game NFT missing from UTXO input" (assetClassValueOf (txOutValue ownInput) (gNFT game) == 1) &&
    case (dat, red) of 
        --guesser makes two choices c1 and c2
        --game CONTINUES
        (GameDatum bs [Nothing, Nothing], Guess [c1, c2]) ->
            traceIfFalse "not signed by guesser"        (txSignedBy info (unPaymentPubKeyHash $ gGuesser game))                     && 
            traceIfFalse "challenger's stake missing"   (lovelaces (txOutValue ownInput) == gStake game)                            &&
            traceIfFalse "guesser's stake missing"      (lovelaces (txOutValue ownOutput) == (2 * gStake game))                     &&
            traceIfFalse "invalid output datum"         (outputDatum == GameDatum bs [Just c1, Just c2])                            &&
            traceIfFalse "missed guess deadline"        (to (gGuessDeadline game) `contains` txInfoValidRange info)                 &&
            traceIfFalse "game NFT is missing"          (assetClassValueOf (txOutValue ownOutput) (gNFT game) == 1)

        --guesser's choices c1 and c2 were both incorrect
        --challenger proves choice c was chosen instead of c1 and c2
        --game OVER
        (GuessDatum bs [Just c1, Just c2], Prove nonce c) ->
            traceIfFalse "not signed by challenger"      (txSignedBy info (unPaymentPubKeyHash $ gChallenger game))                 &&   
            traceIfFalse "invalid proof"                 (isvalidProof bs nonce c c1 c2)                                            &&    
            traceIfFalse "missed prove deadline"         (to (gProveDeadline game) `contains` txInfoValidRange info)                &&
            traceIfFalse "invalid output datum"          (outputDatum == GameDatum bs [Just c1, Just c2])                           &&
            traceIfFalse "invalid stake"                 (lovelaces (txOutValue ownInput) == (2 * gStake game))                     &&
            traceIfFalse "NFT must go to challenger"      nftToFirst

        --guesser no longer responds
        --challenger gets the stake back
        (GameDatum _ [Nothing, Nothing], ClaimChallenger) ->
            traceIfFalse "not signed by challenger"      (txSignedBy info (unPaymentPubKeyHash $ gChallenger game))                 &&    
            traceIfFalse "too early"                     (from (1 + gGuessDeadline game) `contains` txInfoValidRange info)          &&
            traceIfFalse "stake missing"                 (lovelaces (txOutValue ownInput) == gStake game)                           &&                                                              
            traceIfFalse "NFT must go to challenger"      nftToFirst


        -- challenger no longer responds. guesser WINS.   
        -- game OVER
        (GameDatum _, [_, _], ClaimGuesser) ->                                                                  
            traceIfFalse "not signed by guesser"         (txSignedBy info (unPaymentPubKeyHash $ gGuesser game))                    &&  
            traceIfFalse "too early"                     (from (1 + gProveDeadline game) `contains` txInfoValidRange info)          &&
            traceIfFalse "invalid stake"                 (lovelaces (txOutValue ownOutput) == (2 * gStake game))                    &&
            traceIfFalse "NFT must go to challenger"      nftToFirst

        _ -> False

     where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        ownInput :: TxOut
        ownInput = case findOwnInput ctx of
            Nothing -> traceError "game input missing"
            Just i  -> txInInfoResolved i

        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of
            [o] -> o
            _   -> traceError "expected exactly one game output" 


        outputDatum :: GameDatum
        outputDatum = case gameDatum $ txOutDatumHash ownOutput >>= flip findDatum info of
            Nothing -> traceError "game output datum not found"
            Just d  -> d
        
        isvalidProof :: BuiltinByteString -> BuiltinByteString -> GameChoice -> GameChoice -> GameChoice -> Bool
        isValidProof bs nonce c c1 c2 = sha_256 (nonce `appendByteString` $unsafeFromGameChoice c) == bs && c /= c1 && c /= c2

        nftToChallenger :: Bool
        nftToChallenger = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ gChallenger game) (gNFT game) == 1 


data Gaming
    instance Scripts.ValidatorTypes Gaming where
        type instance DatumType Gaming = GameDatum
        type instance RedeemerType Gaming = GameRedeemer

    
typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
        ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsA
        `PlutusTx.applyCode` PlutusTx.liftCode bsB
        `PlutusTx.applyCode` PlutusTx.liftCode bsC
        `PlutusTx.applyCode` PlutusTx.liftCode bsD)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer


gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator    