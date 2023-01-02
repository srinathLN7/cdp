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
    { gFirst          :: !PaymentPubKeyHash
    , gSecond         :: !PaymentPubKeyHash
    , gStake          :: !Integer
    , gDeadline       :: !POSIXTime
    , gToken          :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Game

newtype SecretWord = SW BuiltinByteString 
                    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq)

instance Eq SecretWord where
     {-# INLINABLE (==) #-}
     sw == sw   = True
     _ == _     = False 

PlutusTx.unstableMakeIsData ''SecretWord

newtype GuessWord = GW BuiltinByteString 
                    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq)

instance Eq GuessWord where
     {-# INLINABLE (==) #-}
     gw == gw   = True
     _ == _     = False 

PlutusTx.unstableMakeIsData ''GuessWord

data GameDatum = GameDatum SecretWord (Maybe GuessWord) 
                deriving Show


instance Eq GameDatum where 
    {-# INLINABLE (==) #-}
    GameDatum sw (mgw) == GameDatum sw' (mgw') = (sw == sw') && (mgw == mgw')

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer =     Set SecretWord 
                      | Construct GuessWord 
                      | Reveal BuiltinByteString 
                      | ClaimFirst 
                      | ClaimSecond
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
