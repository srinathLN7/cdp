# psuedo code

P1: Challenger 
P2: Guesser

P1: **STARTGAME**
		- initChallenge and set secret word
		- put stake amt
		- sign Tx and submit BEFORE deadline
P2: accept/reject challenge
	if accept:
		- put stake amt
		- sign Tx and submit BEFORE deadline
	else
		- do nothing until deadline
P1: check P2 response
	if response
		- reveal validHint
		- sign Tx and submit BEFORE deadline
		  **CONTINUEGAME**
	else 
		- claim stake back
		- sign Tx and submit BEFORE deadline
		  **ENDGAME** 
P2: Label `playG` 
		check `n_attempt <= n_max_attempt`
		if yes 
		 	do
			- construct guess word
			- sign Tx and submit BEFORE deadline 
				if onChainValidation pass
					case n_attempt=1 -> claimFullReward  P2: wins
					case n_attempt=2 -> claimStakeBack   Draw
					 **ENDGAME**
				else 
					Repeat `playG`
		else 
			do nothing until deadline
P1: wins
		-claimFullReward
		 **ENDGAME**
