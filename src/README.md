# A brief description of the source code 

We call this code `FiftyFifty` since both players have equal chances of winning or getting their stake back.

## On-Chain code

First, we create the data type `Game` that will be used as the parameter for the contract (44-51). We define challenger `C` and guesser `G` with their public key hashes. We also define their stake, the guessing deadline and the proving deadline. In the end we define `gNFT` an NFT token. Since the game contains changing state and the UTXOs together with their datums are immutable we need to create a new UTXO every time the state of the game is changing. And to be able to connect the old UTXO with the new one we can use an NFT that exists only once and gets assigned to the new UTXO every
time the state of the game changes. Another reason we decide to use an NFT is that another UTXO at the same address with the same datum could easily be created which could potentially disturb the game. In order to uniquely identify the UTXO carrying our game state we mint an NFT and attach it to our transaction inputs. 

The type `GameChoice` defines the four possible options (`A|B|C|D`) the challenger can choose and then we derive the plutus equality for the `GameChoice` type so that they can be compared (55-64). We will use the `GameDatum` as state information for the contract (68-70). The `BuiltinByteString` is the hash that the challenger `C` submits and the two `Maybe GameChoice` refers to the two choice of the guesser `G`. It’s a `Maybe` because in the beginning `G` has not yet guessed. `FinishedAsDraw` refers to the state where `C` proves that first guess made by `G` is incorrect and declares the game as draw with a valid proof. We also implement the Plutus equality for the game datum (72-76). 

Next, we implement the game redeemer with the options that corresponds to our player actions (80-86). `Guess` means `G` makes two valid guesses. `ProveF` is for the case when both guesses made by `G` are incorrect and `C` wants to prove it to unlock all the rewards. `ProveP` refers to the case where the first guess of `G` is incorrect but the second guess of`G` is correct. In this case `C` provides a valid proof to get his own stake back. `ClaimChallenger` is the case when `G` does not move and `C` decides to claim back the stake. `ClaimGuesser` is for the case when `C` has already invoked `ProveP` after `G` made the guess, thereby allowing `G` to claim his stake back. `ClaimFullReward` is for the situation where `C` stops responding after `G` made his guesses which allows `G` to unlock all the rewards locked to the script address.

We define a few helper functions to make our life easier (90-110). The crux of the on-chain validation logic is defined in the `mkGameValidator` function which is our onchain validator (113-208). It validates for each of the aforementioned case specified above. Note in all the cases, we check if the input we are validating contains `gNFT` (116) to ensure that we are validating for the same game. 


## Off-Chain code

For the off-chain code we first define two helper functions. The `findGameOutput` function takes a `Game` type parameter as input and then in the contract monad tries to find the UTXO (233-242). Since this opeartion could potentially fail we return a `Maybe` type. In the output, we return the transaction output reference, the transaction output chain index and the game datum as a triple. Our second function `waitUntilTimeHasPassed` we take in a POSIX time and waits until that time has passed plus for 1 more slot (244-250).

We then define the respective parameters for the challenger (253-262) and guesser(328-337). The `challengerGame` function takes in `ChallengerParams` and first initiates the challenge and then waits until the guessing deadline is passed (265-284). After that, it constructs the appropriate transaction depending on how the game unfolds (286-325). Similarly the `guesserGame` takes in `guesserParams` and first makes the two guesses and then waits until the proving deadline is passed (340-372). After that it constructs the transcation depending on how the game progressed (374-402). 

In the end we define the schema and define the endpoints contract (404-410).
