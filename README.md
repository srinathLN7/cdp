# ada-guess

`ada-guess` is a simple game inspired from the British television quiz show [Who Wants to Be a Millionaire?](https://en.wikipedia.org/wiki/Who_Wants_to_Be_a_Millionaire%3F_(British_game_show))
It consists of two players  -`C` the challenger and `G` the guesser. `C` chooses an option out of four possible choices `A`,`B`,`C`,`D` and challenges `G` to guess the secret choice. `G` can make two choices. For example 'A, B' means that `G` thinks option `A` is most likely the correct answer and if not, then falls back to option `B`. The three possible outcomes are tabularized below:

| Challenger  |  Guesser    |    Result     |
| :---        |    :----:   |          ---: |
|    `A`      |    `A, B`   |    `G` wins   |
|    `B`      |    `A, B`   |    `DRAW`     |
|    `C`      |    `B, D`   |    `C` wins   |


The distribution of the rewards are based on the following rules:
* In case of a successful first attempt, `G` wins and claims the entire `ADA` locked to the contract script. 
* In case of a successful second attempt, `C` and `G` can claim their own respective stake back. 
* Otherise, `C` wins and claims the entire `ADA` locked to the contract script.

As one can infer, both `C` and `G` have 25% probability of winning the game and 50% chance of claiming their original stake.

## Game Flow

```mermaid
stateDiagram-v2 
   [*] --> hash(secret_choice)||nonce : challenger
   hash(secret_choice)||nonce --> guess_secret_choice : guesser
   hash(secret_choice)||nonce --> UTXO: claimChallengerStake
   guess_secret_choice --> UTXO: claimGuesserStake
   guess_secret_choice --> UTXO: claimFullReward
   hash(secret_choice)||nonce --> UTXO: proveF
   hash(secret_choice)||nonce --> UTXO: proveP 
   UTXO --> [*] : Script
```

## How `ada-guess` works?

* Once the challenge is initiated, `G` must respond within a certain deadline known as the guess deadline. If `G` does not respond within this
  time, then `C` can claim his/her own stake back `claimChallengerStake`.
* Incase, both the choices of `G` are wrong, `C` must provide a valid proof to the blockchain `proveF` within a certain deadline known as the prove deadline to unlock the entire reward.
* Incase, only the first choice of `G` is wrong, then `C` still must provide a valid proof to the blockchain `proveP` within prove deadline to unlock his/her own stake back. 
* Incase, `C` does not respond after `G` makes the guess, `G` can unlock the entire reward anytime after the prove deadline.


## Code

A brief description of the source code and how to test the same can be found [here.](https://github.com/srinathLN7/cdp/tree/main/src)

e-UTXO diagram can be found [here.](https://github.com/srinathLN7/cdp/blob/main/img/eutxo.jpg) 







