{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module TestFiftyFifty
    ( test
    , test'
    , GameChoice (..)
    ) where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Ledger
import           Ledger.TimeSlot
import           Ledger.Value
import           Ledger.Ada                 as Ada
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, Show (..))
import           Wallet.Emulator.Wallet

import           FiftyFifty

test :: IO ()
test = do
    test' A A B
    test' A B A
    test' A B C

w1, w2 :: Wallet
w1 = knownWallet 1
w2 = knownWallet 2

test' :: GameChoice -> GameChoice -> GameChoice -> IO ()
test' c c1 c2 = runEmulatorTraceIO' def emCfg $ myTrace c c1 c2
  where
    emCfg :: EmulatorConfig
    emCfg = def { _initialChainState = Left $ Map.fromList
                    [ (w1, v <> assetClassValue (AssetClass (ffCurrency, ffToken)) 1)
                    , (w2, v)
                    ]
                }

    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000

ffCurrency :: CurrencySymbol
ffCurrency = "5050"

ffToken :: TokenName
ffToken = "FF"

myTrace :: GameChoice -> GameChoice -> GameChoice -> EmulatorTrace ()
myTrace c c1 c2 = do
    Extras.logInfo $ "Challenger set " ++ show c ++ ", Guesser first guess: " ++ show c1 ++ ", Guesser second guess: " ++ show c2

    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints

    let pkh1      = mockWalletPaymentPubKeyHash w1
        pkh2      = mockWalletPaymentPubKeyHash w2
        stake     = 100_000_000
        deadline1 = slotToBeginPOSIXTime def 5
        deadline2 = slotToBeginPOSIXTime def 10

        cp = ChallengerParams
                { cpGuesser        = pkh2
                , cpStake          = stake
                , cpGuessDeadline  = deadline1
                , cpProveDeadline  = deadline2
                , cpNonce          = "2083236893"
                , cpCurrency       = ffCurrency
                , cpTokenName      = ffToken
                , cpChoice         = c
                }
        gp = GuesserParams
                { gpChallenger     = pkh1
                , gpStake          = stake
                , gpGuessDeadline  = deadline1
                , gpProveDeadline  = deadline2
                , gpCurrency       = ffCurrency
                , gpTokenName      = ffToken
                , gpChoice1        = c1
                , gpChoice2        = c2
                }

    callEndpoint @"challenge" h1 cp

    void $ Emulator.waitNSlots 3

    callEndpoint @"guess" h2 gp

    void $ Emulator.waitNSlots 10
