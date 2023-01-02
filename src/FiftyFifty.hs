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
    { gFirst           :: !PaymentPubKeyHash
    , gSecond          :: !PaymentPubKeyHash
    , gStake           :: !Integer
    , gGuessDeadline1   :: !POSIXTime
    , gGuessDeadline2   :: !POSIXTime
    , gProveDeadline1  :: !POSIXTime
    , gProveDeadline2  :: !POSIXTime
    , gNFT             :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Game

data GameChoice = A | B | C | D
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

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
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer =   Guess GameChoice 
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

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> GameDatum -> GameRedeemer -> ScriptContext -> Bool 
mkGameValidator game dat red ctx = 
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gNFT game) == 1) &&
    case (dat, red) of 
        --guesser makes first attempt
        -- game CONTINUES
        (GameDatum bs [Nothing, Nothing], Guess c) ->
            traceIfFalse "not signed by guesser"        (txSignedBy info (unPaymentPubKeyHash $ gSecond game))                      && 
            traceIfFalse "challenger's stake missing"   (lovelaces (txOutValue ownInput) == gStake game)                            &&
            traceIfFalse "guesser's stake missing"      (lovelaces (txOutValue ownOutput) == (2 * gStake game))                     &&
            traceIfFalse "invalid output datum"         (outputDatum == GameDatum bs [Just c, Nothing])                             &&
            traceIfFalse "missed first guess deadline"   (to (gGuessDeadline1 game) `contains` txInfoValidRange info)               &&
            traceIfFalse "game NFT is missing"          (assetClassValueOf (txOutValue ownOutput) (gNFT game) == 1)

        -- guesser first attempt (choice c') is wrong
        -- challenger proving it that c' was not chosen
        -- game CONTINUES
        (GameDatum bs [Just c', Nothing], Prove nonce c) ->
            traceIfFalse "not signed by challenger"      (txSignedBy info (unPaymentPubKeyHash $ gFirst game))                      &&                
            traceIfFalse "invalid proof"                 (isvalidProof bs nonce c)                                                  &&
            traceIfFalse "missed first prove deadline"   (to (gProveDeadline1 game) `contains` txInfoValidRange info)               &&
            traceIfFalse "invalid output datum"          (outputDatum == GameDatum bs [Just c', Nothing])                           &&
            traceIfFalse "invalid stake"                 (lovelaces (txOutValue ownInput) == (2 * gStake game))                     &&
            traceIfFalse "game NFT is missing"           (assetClassValueOf (txOutValue ownOutput) (gNFT game) == 1)

        -- guesser makes second attempt
        -- game CONTINUES
        (GameDatum bs [Just c', Nothing], Guess c) ->
            traceIfFalse "not signed by guesser"        (txSignedBy info (unPaymentPubKeyHash $ gSecond game))                      && 
            traceIfFalse "invalid stake"                (lovelaces (txOutValue ownOutput) == (2 * gStake game))                     &&
            traceIfFalse "invalid output datum"         (outputDatum == GameDatum bs [Just c', Just c])                             &&
            traceIfFalse "missed second guess deadline"  (to (gGuessDeadline2 game) `contains` txInfoValidRange info)               &&
            traceIfFalse "game NFT is missing"          (assetClassValueOf (txOutValue ownOutput) (gNFT game) == 1)

        -- guesser second attempt (choice c'') is still wrong
        -- challenger proving it that c' was not chosen
        -- game OVER
        (GameDatum bs [Just c', Just c''], Prove nonce c) ->
            traceIfFalse "not signed by challenger"      (txSignedBy info (unPaymentPubKeyHash $ gFirst game))                      &&
            traceIfFalse "invalid proof"                 (isvalidProof bs nonce c)                                                  &&
            traceIfFalse "missed second prove deadline"  (to (gProveDeadline2 game) `contains` txInfoValidRange info)               && 
            traceIfFalse "invalid output datum"          (outputDatum == GameDatum bs [Just c', Just c''])                          &&
            traceIfFalse "invalid stake"                 (lovelaces (txOutValue ownOutput) == (2 * gStake game))                    &&
            traceIfFalse "NFT must go to challenger"     nftToFirst

        -- guesser no longer responds
        -- game OVER
        (GameDatum _ [Nothing, Nothing], ClaimChallenger) ->
            traceIfFalse "not signed by challenger"      (txSignedBy info (unPaymentPubKeyHash $ gFirst game))                      &&    
            traceIfFalse "too early"                     (from (1 + gGuessDeadline2 game) `contains` txInfoValidRange info)         &&
            traceIfFalse "stake missing"                 ( (lovelaces (txOutValue ownInput) == gStake game) 
                                                           || (lovelaces (txOutValue ownInput) == 2 * gStake game) )                &&

            traceIfFalse "NFT must go to challenger"     nftToFirst

        -- challenger no longer responds
        -- game OVER
        (GameDatum _, [_, _], ClaimGuesser) ->                                                                  
            traceIfFalse "not signed by guesser"        (txSignedBy info (unPaymentPubKeyHash $ gSecond game))                      &&  
            traceIfFalse "too early"                     (from (1 + gProveDeadline2 game) `contains` txInfoValidRange info)         &&
            traceIfFalse "invalid stake"                 (lovelaces (txOutValue ownOutput) == (2 * gStake game))                    &&
            traceIfFalse "NFT must go to challenger"     nftToFirst

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
        
         isvalidProof :: BuiltinByteString -> BuiltinByteString -> GameChoice -> Bool
         isValidProof bs nonce c = sha_256 (nonce `appendByteString` c) == bs

         nftToFirst :: Bool
         nftToFirst = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ gFirst game) (gToken game) == 1 