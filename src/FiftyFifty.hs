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
 
module FiftyFifty
    ( Game (..)
    , GameChoice (..)
    , ChallengerParams (..)
    , GuesserParams (..)
    , GameSchema
    , endpoints
    ) where

 
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

 
-- onchain code

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

data GameDatum =   GameDatum BuiltinByteString (Maybe GameChoice) (Maybe GameChoice) 
                 | Draw     
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc1 mc2 == GameDatum bs' mc1' mc2' = (bs == bs') && (mc1 == mc1') && (mc2 == mc2')
    Draw                 == Draw                    = True
    _                    == _                       = False   

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer =   Guess GameChoice GameChoice 
                    | ProveF BuiltinByteString GameChoice
                    | ProveP BuiltinByteString GameChoice
                    | ClaimChallenger 
                    | ClaimGuesser
                    | ClaimFullReward
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
    traceIfFalse "game NFT missing from UTXO-input" (assetClassValueOf (txOutValue ownInput) (gNFT game) == 1) &&
    case (dat, red) of 
        --guesser makes two choices c1 and c2
        --game CONTINUES
        (GameDatum bs Nothing Nothing, Guess c1 c2)         ->
            traceIfFalse "not signed by guesser"        (txSignedBy info (unPaymentPubKeyHash $ gGuesser game))                     && 
            traceIfFalse "challenger's stake missing"   (lovelaces (txOutValue ownInput) == gStake game)                            &&
            traceIfFalse "guesser's stake missing"      (lovelaces (txOutValue ownOutput) == (2 * gStake game))                     &&
            traceIfFalse "invalid output datum"         (outputDatum == GameDatum bs (Just c1) (Just c2))                           &&
            traceIfFalse "missed guess deadline"        (to (gGuessDeadline game) `contains` txInfoValidRange info)                 &&
            traceIfFalse "game NFT is missing"          (assetClassValueOf (txOutValue ownOutput) (gNFT game) == 1)

        --guesser's choices c1 and c2 were both incorrect
        --challenger proves choice c was chosen instead of c1 and c2
        --game OVER - challenger WINS
        (GameDatum bs (Just c1) (Just c2), ProveF nonce c)  ->
            traceIfFalse "not signed by challenger"      (txSignedBy info (unPaymentPubKeyHash $ gChallenger game))                 &&   
            traceIfFalse "invalid proof"                 (isValidFullProof bs nonce c c1 c2)                                        &&    
            traceIfFalse "missed prove deadline"         (to (gProveDeadline game) `contains` txInfoValidRange info)                &&
            traceIfFalse "invalid stake"                 (lovelaces (txOutValue ownInput) == (2 * gStake game))                     &&
            traceIfFalse "NFT must go to challenger"      nftToChallenger

        --guesser's first choice c1 was incorrect but second choice was correct.
        --challenger proves only the second guess was correct and hence reclaims the stake back 
        --challenger leaves the NFT back so that guesser can claim his/her stake back 
        --game OVER with DRAW
        (GameDatum bs (Just c1) (Just c2), ProveP nonce c)  ->
            traceIfFalse "not signed by challenger"      (txSignedBy info (unPaymentPubKeyHash $ gChallenger game))                 &&   
            traceIfFalse "invalid proof"                 (isValidPartialProof bs nonce c c1 c2)                                     &&
            traceIfFalse "invalid output datum"          (outputDatum == Draw)                                                      &&    
            traceIfFalse "missed prove deadline"         (to (gProveDeadline game) `contains` txInfoValidRange info)                &&
            traceIfFalse "invalid UTXO input"            (lovelaces (txOutValue ownInput) == (2*gStake game))                       &&
            traceIfFalse "invalid UTXO output"           (lovelaces (txOutValue ownOutput) == gStake game)                          &&
            traceIfFalse "game NFT is missing"           (assetClassValueOf (txOutValue ownOutput) (gNFT game) == 1)

        --guesser no longer responds
        --challenger gets the stake back
        --game OVER. NO RESULT
        (GameDatum _ Nothing Nothing, ClaimChallenger)      ->
            traceIfFalse "not signed by challenger"      (txSignedBy info (unPaymentPubKeyHash $ gChallenger game))                 &&    
            traceIfFalse "too early"                     (from (1 + gGuessDeadline game) `contains` txInfoValidRange info)          &&
            traceIfFalse "stake missing"                 (lovelaces (txOutValue ownInput) == gStake game)                           &&                                                              
            traceIfFalse "NFT must go to challenger"      nftToChallenger


        -- challenger no longer responds with a valid proof     
        -- game OVER - guesser WINS.
        (GameDatum _ (Just _) (Just _), ClaimFullReward)    ->                                                                  
            traceIfFalse "not signed by guesser"         (txSignedBy info (unPaymentPubKeyHash $ gGuesser game))                    &&  
            traceIfFalse "too early"                     (from (1 + gProveDeadline game) `contains` txInfoValidRange info)          &&
            traceIfFalse "invalid stake"                 (lovelaces (txOutValue ownInput) == (2 * gStake game))                     &&
            traceIfFalse "NFT must go to challenger"      nftToChallenger

        -- challenger has already provided a valid partial proof and declared the game as Draw
        -- guesser can get the stake back
        -- game over with DRAW
        (Draw, ClaimGuesser)                                ->                                                                  
            traceIfFalse "not signed by guesser"         (txSignedBy info (unPaymentPubKeyHash $ gGuesser game))                    &&  
            traceIfFalse "too early"                     (from (1 + gProveDeadline game) `contains` txInfoValidRange info)          &&
            traceIfFalse "invalid stake"                 (lovelaces (txOutValue ownInput) == gStake game)                           &&
            traceIfFalse "NFT must go to challenger"      nftToChallenger


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
        
        isValidFullProof :: BuiltinByteString -> BuiltinByteString -> GameChoice -> GameChoice -> GameChoice -> Bool
        isValidFullProof bs nonce c c1 c2 = sha2_256 (nonce `appendByteString` (unsafeFromGameChoice c))== bs && c /= c1 && c /= c2

        isValidPartialProof :: BuiltinByteString -> BuiltinByteString -> GameChoice -> GameChoice -> GameChoice -> Bool
        isValidPartialProof bs nonce c c1 c2 = sha2_256 (nonce `appendByteString` (unsafeFromGameChoice c))== bs && c /= c1 && c == c2

        nftToChallenger :: Bool
        nftToChallenger = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ gChallenger game) (gNFT game) == 1 


data Gaming
instance Scripts.ValidatorTypes Gaming where
    type instance DatumType Gaming      = GameDatum
    type instance RedeemerType Gaming   = GameRedeemer

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer


gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator


-- Off-chain
findGameOutput :: Game -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, GameDatum))
findGameOutput game = do
    utxos <- utxosAt $ gameAddress game
    return $ do
        (oref, o) <- find f $ Map.toList utxos
        dat       <- gameDatum $ either (const Nothing) Just $ _ciTxOutDatum o
        return (oref, o, dat)
  where
    f :: (TxOutRef, ChainIndexTxOut) -> Bool
    f (_, o) = assetClassValueOf (_ciTxOutValue o) (gNFT game) == 1

waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = do
    s1 <- currentSlot
    logInfo @String $ "current slot: " ++ show s1 ++ ", waiting until " ++ show t
    void $ awaitTime t >> waitNSlots 1
    s2 <- currentSlot
    logInfo @String $ "waited until: " ++ show s2


data ChallengerParams = ChallengerParams
    { cpGuesser         :: !PaymentPubKeyHash
    , cpStake           :: !Integer
    , cpGuessDeadline   :: !POSIXTime
    , cpProveDeadline   :: !POSIXTime
    , cpNonce           :: !BuiltinByteString
    , cpCurrency        :: !CurrencySymbol
    , cpTokenName       :: !TokenName
    , cpChoice          :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)


challengerGame :: forall w s. ChallengerParams -> Contract w s Text ()
challengerGame cp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let game = Game 
              { gChallenger     = pkh
              , gGuesser        = cpGuesser cp
              , gStake          = cpStake cp
              , gGuessDeadline  = cpGuessDeadline cp
              , gProveDeadline  = cpProveDeadline cp
              , gNFT            = AssetClass (cpCurrency cp, cpTokenName cp)
              }
        v  = lovelaceValueOf (cpStake cp) <> assetClassValue (gNFT game) 1
        c  = cpChoice cp
        bs = sha2_256 $ cpNonce cp `appendByteString` (unsafeFromGameChoice c)
        tx = Constraints.mustPayToTheScript (GameDatum bs Nothing Nothing) v
    ledgerTx <- submitTxConstraints (typedGameValidator game) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "[challenger] initiated challenge - set choice: " ++ show (cpChoice cp)

    waitUntilTimeHasPassed $ cpGuessDeadline cp

    m   <- findGameOutput game 
    now <- currentTime
    case m of
         Nothing             -> throwError "[challenger] no game output found"
         Just (oref, o, dat) -> case dat of
                GameDatum _ Nothing Nothing                                 -> do
                    logInfo @String "[challenger] guesser did not play"
                    let lookups = Constraints.unspentOutputs (Map.singleton oref o)                                                     <>
                                  Constraints.otherScript (gameValidator game)
                        tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimChallenger)            <>
                                  Constraints.mustValidateIn (from now)
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                    logInfo @String "[challenger] reclaimed stake" 

                GameDatum _ (Just c1) (Just c2) | (c1 /= c && c2 == c)      -> do 
                    logInfo @String "[challenger] guesser first guessed incorrectly but guessed correctly second time"
                    let token   = assetClassValue (gNFT game) 1
                    let v'      = let x = lovelaceValueOf (cpStake cp) in x <> token
                    let lookups = Constraints.unspentOutputs (Map.singleton oref o)                                                     <>
                                  Constraints.otherScript (gameValidator game)                                                          <>
                                  Constraints.typedValidatorLookups (typedGameValidator game)
                        tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ ProveP (cpNonce cp) c)    <>
                                  Constraints.mustPayToTheScript (Draw) v'                                                              <>  
                                  Constraints.mustValidateIn (to $ now + 1000)
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                    logInfo @String "[challenger] GAME OVER - DRAW!!! Challenger claimed stake back"    

                GameDatum _ (Just c1) (Just c2) | (c1 /= c && c2 /= c)      -> do 
                    logInfo @String "[challenger] guesser guessed incorrectly twice"
                    let lookups = Constraints.unspentOutputs (Map.singleton oref o)                                                     <>
                                  Constraints.otherScript (gameValidator game)                                                          
                        tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ ProveF (cpNonce cp) c)    <>
                                  Constraints.mustValidateIn (to $ now + 1000)
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                    logInfo @String "[challenger] GAME OVER - CHALLENGER WON!!!"
                
                _ -> logInfo @String "[challenger] GAME OVER - GUESSER WON!!!"       


data GuesserParams = GuesserParams
    { gpChallenger     :: !PaymentPubKeyHash
    , gpStake          :: !Integer
    , gpGuessDeadline  :: !POSIXTime
    , gpProveDeadline  :: !POSIXTime
    , gpCurrency       :: !CurrencySymbol
    , gpTokenName      :: !TokenName
    , gpChoice1        :: !GameChoice
    , gpChoice2        :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)


guesserGame :: forall w s. GuesserParams -> Contract w s Text ()
guesserGame gp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let game = Game 
               { gChallenger    = gpChallenger gp
               , gGuesser       = pkh
               , gStake         = gpStake gp
               , gGuessDeadline = gpGuessDeadline gp
               , gProveDeadline = gpProveDeadline gp
               , gNFT           = AssetClass (gpCurrency gp, gpTokenName gp)
               }
    m <- findGameOutput game

    case m of
            Just (oref, o, GameDatum bs Nothing Nothing) -> do
                logInfo @String "[guesser] running guessing game found"
                now <- currentTime
                let token   = assetClassValue (gNFT game) 1
                let v       = let x = lovelaceValueOf (gpStake gp) in x <> x <> token
                    c1      = gpChoice1 gp
                    c2      = gpChoice2 gp
                    lookups = Constraints.unspentOutputs (Map.singleton oref o)                                             <>
                              Constraints.otherScript (gameValidator game)                                                  <>
                              Constraints.typedValidatorLookups (typedGameValidator game)
                    tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Guess c1 c2)      <>
                              Constraints.mustPayToTheScript (GameDatum bs (Just c1) (Just c2)) v                           <>
                              Constraints.mustValidateIn (to now)
                ledgerTx <- submitTxConstraintsWith @Gaming lookups tx
                let tid = getCardanoTxId ledgerTx
                void $ awaitTxConfirmed tid
                logInfo @String $ "[guesser] guessed choices: " ++ show (gpChoice1 gp) ++ " and " ++ show (gpChoice2 gp) 

                waitUntilTimeHasPassed $ gpProveDeadline gp

                m'   <- findGameOutput game
                now' <- currentTime

                case m' of
                    Nothing                 ->    logInfo @String "[guesser] GAME OVER - CHALLENGER WON!!!"

                    Just (oref', o', Draw)  -> do
                        logInfo @String "[guesser] challenger provided proof that first guess was incorrect and declared the game as DRAW"
                        let lookups' =  Constraints.unspentOutputs (Map.singleton oref' o')                                             <>
                                        Constraints.otherScript (gameValidator game)
                            tx'      =  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ClaimGuesser)        <>
                                        Constraints.mustValidateIn (from now')                                                          <>
                                        Constraints.mustPayToPubKey (gpChallenger gp) (token <> adaValueOf (getAda minAdaTxOut))
                        ledgerTx' <- submitTxConstraintsWith @Gaming lookups' tx'
                        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                        logInfo @String "GAME OVER - DRAW!!! Guesser claimed stake back"

                    Just (oref', o', _)     -> do
                        logInfo @String "[guesser] challenger didn't provide any valid proof"
                        let lookups' =  Constraints.unspentOutputs (Map.singleton oref' o')                                             <>
                                        Constraints.otherScript (gameValidator game)
                            tx'      =  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ClaimFullReward)     <>
                                        Constraints.mustValidateIn (from now')                                                          <>
                                        Constraints.mustPayToPubKey (gpChallenger gp) (token <> adaValueOf (getAda minAdaTxOut))
                        ledgerTx' <- submitTxConstraintsWith @Gaming lookups' tx'
                        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                        logInfo @String "[guesser] GAME OVER - GUESSER WON!!!"     

            _ -> logInfo @String "no running game found"     

type GameSchema = Endpoint "challenge" ChallengerParams .\/ Endpoint "guess" GuesserParams

endpoints :: Contract () GameSchema Text ()
endpoints = awaitPromise (challenge `select` guess) >> endpoints
  where
    challenge  = endpoint @"challenge"  challengerGame
    guess      = endpoint @"guess" guesserGame           