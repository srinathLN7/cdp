# How the code works?

## On-Chain code

First, we create the data type `Game` that will be used as the parameter for the contract (44-51). We define `C` and `G` with their public key hashes. We also define their stake, the guessing deadline and the proving deadline. In the end we define a NFT token. Since the game contains some state that is changing and UTXOs together with their datums are immutable we need to create a new UTXO
every time the state of the game is changing. And to be able to connect the old UTXO with the new one we can use an NFT that exists only once and gets assigned to the new UTXO every
time the state changes. We then call this token `gNFT` game NFT. Another reason we decide to use an NFT is that somebody could create another UTXO at the same address with the same datum and would try
to disturb the game. So in order to uniquely identify our UTXO with which the game is started we use an NFT that exists only once. 

The type `GameChoice` defines the four possible options that the challenger can choose and then we derive the plutus equality for the game choice type so that they can be compared (55-64). We will use the game datum as state information for the contract (68-70). The `BuiltinByteString` is the hash that `C` submits and the two `Maybe GameChoice` refers to the two choice of `G`. It’s a
maybe because in the beginning `G` has not yet guessed. `FinishedAsDraw` refers to the state where `C` proves that first guess made by `G` is incorrect and declares the game as draw with a valid proof. We implement also the Plutus equality for the game datum (72-76). 

Next we implement the game redeemer with the options that corresponds to our player actions (80-86). `Guess` means `G` makes two valid guesses. `ProveF` is for the case when both guesses made by `G` are incorrect and `C` wants to prove it and claim the rewards. `ProveP` refers to the case where the first guess of `G` is incorrect but the second guess of`G` is correct. In this case `C` provides a valid proof to get his own stake back. `ClaimChallenger` is the case when `G` does not move and `C` decides to claim back the stake. `ClaimGuesser` is for the case when `C` has already invoked `ProveP` after `G` made the guess to claim his own stake back and now `G` can claim his  stake back. `ClaimFullReward` is for the situation where `C` stops responding after `G` made his guesses and hence can unlock the full reward locked to the script address.

We define a few helper functions to make our life easier (90-110). The crux of the on-chain validation logic is defined in the `mkGameValidator` function which is our onchain validator (113-208). It validates for each of the aforementioned case specified above. Note in all the cases, we check if the input we are validating contains `gNFT` (116) to ensure that we are playing the same game. 


## Off-Chain code

For the off-chain code we first define two helper functions. The `findGameOutput` function takes as input a game type parameter and then in the contract monad tries to find the UTXO (233-242). Since it could fail we are returning a maybe type. We return the transaction output reference, the transaction output chain index and the game datum as a triple. Our second function `waitUntilTimeHasPassed` we take in a posix time and wait until that time has passed plus for 1 more slot (244-250).

We then define the respective parameters for the challenger and guesser. The `challengerGame` function takes in `ChallengerParams` and constructs the transaction for the challenger and submits it to the blockchain. The function first initiates the challenge and then waits until the guessing deadline is passed. After that, it constructs the appropriate transaction depending on how the game unfolds. Similarly the `guesserGame` takes in `guesserParams` and first makes the two guesses. It waits until the proving deadline is passed adn then constructs the transcation depending on how the game progressed. In the end we define the schema and define the endpoints contract.
