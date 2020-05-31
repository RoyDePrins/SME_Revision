/*
*	This version is the "World" version. 
*	Meaning that the probability of each world must be given.
* 	Only supports standard SME Revision using BC and SH,
*	no modifications are implemented.
*	In oirder to do this: Use SWI-Prolog code
*/

:- use_module(library(lists)).
:- use_module(library(string)).


/****************************
*****************************
*****************************
Main Function + Checks + useful functions
*****************************
*****************************
****************************/

/*
*	Main function to run the program.
*/
main():-
	% Belief State: 		[(world1,prob1),(w2,p2),...(wn,pn)]
	Belief_State = [('00',3/10),('01',0),('10',5/10),('11',2/10)],
	% List of evidence:		[world1, world2, ..., wn]
	Evidence = ['00','10'],
	% The used SME Revision method: {'SWITCH', 'THETA'}
	Method = 'SWITCH',
	% Perform SME on the given Belief State
	calculate_sme(Belief_State, Evidence, Method, New_Belief_State).


/****************************
*****************************
*****************************
CALCULATE SME
*****************************
*****************************
****************************/

/*
*	Perform SME
*/
calculate_sme(Belief_State, Evidence, Method, New_Belief_State):-
	% Check if the given Belief State is an actual belief state
	checkProbabilitySum(Belief_State),
	% Calculate B(alpha)
	calc_b_alpha(Belief_State, Evidence, B_Alpha),
	(
			Method == 'SWITCH',
			B_Alpha > 0,
			New_Sigma_Method = 'BC'
			
	;(
			Method == 'SWITCH',
			B_Alpha =< 0,
			New_Sigma_Method = 'SH'

	;(
			Method == 'THETA',			
			New_Sigma_Method = 'THETA'
	;
			Method \== 'SWITCH',
			Method \== 'THETA',
			writenl('Wrong SME_method'),
			false
	))),
	% Calculate the normailization factor
	gamma_general(Belief_State, Evidence, New_Sigma_Method, B_Alpha, Gamma),
	% Calculate the new belief state
	calc_bs_general(Belief_State, Belief_State, Evidence, New_Sigma_Method, Gamma, B_Alpha, [], New_Belief_State),
	% Check if the new belief state is an actual belief state
	checkProbabilitySum(New_Belief_State),
	writenl('The new belief state is' , New_Belief_State).

/*
*	Check if the Sum of all probabilities in the given Belief State add to 1
*/
checkProbabilitySum(Belief_State):-
	getSumProbabilities(Belief_State,0,Sum),
	abs(1 - Sum) < float(0.01).

/*
*	Calculate the Sum of all probabilities in a given list
*/
getSumProbabilities([],Sum,Sum).
getSumProbabilities([(_,P)|Tail], Sum, Total):-
	NewSum is Sum + P,
	getSumProbabilities(Tail,NewSum,Total).


/*
*	Calculate the degree of belief in evidence alpha
*/
calc_b_alpha(Belief_State, Evidence, B_Alpha) :- 
	getSatisfyingWorlds(Belief_State, Evidence, SatWorlds),
	getSumProbabilities(SatWorlds, 0, B_Alpha).


/****************************
*****************************
*****************************
Sigma (similarity) Functions
*****************************
*****************************
****************************/
calculate_sigma(CurrentWorld, OtherWorld, B_Alpha, Sigma_Method, Evidence, Sigma):-
	(	
		Sigma_Method == 'BC',
		sigma_bc(CurrentWorld, OtherWorld, Sigma)
	;(	
		Sigma_Method == 'SH',
		sigma_sh(CurrentWorld, OtherWorld, Evidence, Sigma)
	;(
		Sigma_Method == 'THETA',
		sigma_theta(CurrentWorld, OtherWorld, B_Alpha, Evidence, Sigma)
	;
		Sigma_Method \== 'BC',
		Sigma_Method \== 'SH',
		Sigma_Method \== 'THETA',
		writenl('Wrong Sigma Method'),
		false
	))).

/*
*	Calculate the similarity when using Shepard
*/
%Both worlds are equal
sigma_sh(World1, World2, _, 1):-
	World1 == World2.

%Both worlds satisfy the evidence
sigma_sh(World1, World2, Evidence, Sigma):-
	World1 \== World2,
	satisfies(World1, Evidence),
	satisfies(World2, Evidence),
	distance(World1, World2, Dist),
	Sigma is e ** (-Dist).

% At least one of the worlds does not satisfy the evidence
sigma_sh(World1, World2, Evidence, Sigma):-
	World1 \== World2,
	(	
		\+satisfies(World1, Evidence),
		satisfies(World2, Evidence)

	;(
		satisfies(World1, Evidence),
		\+satisfies(World2, Evidence)
	;	
		\+satisfies(World1, Evidence),
		\+satisfies(World2, Evidence)
	)),
	
	distance(World1, World2, Dist),
	max_distance(World1, Max_Dist),
	Power is -Dist - Max_Dist,
	Sigma is e ** Power.

/*
*	Calculate the similarity using Bayesian Conditioning
*/
% Both worlds are the same
sigma_bc(World1, World2, 1):-
	World1 == World2.
% Both worlds differ
sigma_bc(World1, World2, 0):-
	World1 \== World2.

/*
*	Calculate the similarity using Theta and SH
*/
sigma_theta(World1, World2, B_Alpha, Evidence, Sigma):-
	sigma_bc(World1, World2, Sigm_bc),
	sigma_sh(World1, World2, Evidence, Sigm_sh),
	Sigma is B_Alpha * Sigm_bc + (1 - B_Alpha) * Sigm_sh.


/****************************
*****************************
*****************************
Distance Functions
*****************************
*****************************
****************************/



/*
*	Calculate the distance between two worlds
*/
distance(World1, World1, _, 0).
distance(World1, World2, Dist) :-	
	str2lst(World1,W1List),
	str2lst(World2,W2List),	
	getDistanceHamming(W1List, W2List, 0, Dist).
/*
*	Calculate the Hamming Distance between two given worlds
*/
getDistanceHamming([],[],Dist, Dist).
getDistanceHamming([H1|T1],[H2|T2],Dist, Dist2):-	
	(
		H1 == H2, 
		NewDist is Dist 
	; 
		H1 \== H2,
		NewDist is Dist + 1
	),
	getDistanceHamming(T1, T2, NewDist, Dist2).

/*
*	Calculate the maximum distance between worlds in this belief state
*/
max_distance(World, WorldLength):-
	str2lst(World,WList),
	getLength(WList,0,WorldLength).

/*
*	Calculate the length of a world
*	Must do the calculated length '-2', because both quotes around the world name are also counted
*/
getLength([],Length,NewLength):-
	NewLength is Length-2.

getLength([_|Rest],Length,LengthAcc):-
	NewLength is Length + 1,
	getLength(Rest,NewLength,LengthAcc).



/****************************
*****************************
*****************************
Gamma (normalization) functions
*****************************
*****************************
****************************/

/*
*	General formula to calculate the normalization factor
*/
gamma_general(Belief_State, Evidence, Method, B_Alpha, Gamma) :-
	getSatisfyingWorlds(Belief_State, Evidence, SatisfyingWorlds),
	loopOverWorlds(Belief_State, SatisfyingWorlds, Evidence, Method, B_Alpha, 0, Gamma).


/*
*	Loop over the worlds and for each world calculate the calc_gamma_term_world and sum them all
*/
loopOverWorlds(_,[], _, _, _, Gamma, Gamma).
loopOverWorlds(Belief_state,[(W,_)|Tail1], Evidence, Method, B_Alpha, Start, Gamma):-
	calc_gamma_term_World(Belief_state, Belief_state, Evidence, W, Method, B_Alpha, 0, Gamma_Term),
	NewStart is Start + Gamma_Term,
	loopOverWorlds(Belief_state, Tail1, Evidence, Method, B_Alpha, NewStart, Gamma).

/*
*	Calculate the contribution of this world to Gamma as Gamma_Term
*/
calc_gamma_term_World(_,[], _, _, _, _, Gamma_Term, Gamma_Term).
calc_gamma_term_World(Belief_State,[(OtherWorld,ProbOtherW)|Tail2], Evidence, CurrentWorld, Method, B_Alpha, Start, Gamma_Term):-
	calculate_sigma(CurrentWorld, OtherWorld, B_Alpha, Method, Evidence, Sigma),
	NewStart is Start + (ProbOtherW * Sigma),
	calc_gamma_term_World(Belief_State, Tail2, Evidence, CurrentWorld, Method, B_Alpha, NewStart, Gamma_Term).



/****************************
*****************************
*****************************
Satisfaction functions
*****************************
*****************************
****************************/

/*
*	Get all the worlds in this belief state that satisfy the given evidence
*/
getSatisfyingWorlds(Belief_State, Evidence, SatisfyingWorlds):-
	satWorlds(Belief_State, Evidence, [], SatisfyingWorlds).

satWorlds([], _, SatisfyingWorlds, SatisfyingWorlds).
satWorlds([(W,P)|RestWorlds], Evidence, SatisfyingWorlds, SatWorldsAcc):-
	(
			satisfies(W,Evidence),
			append([(W,P)], SatisfyingWorlds, NewSatWorlds),	
			satWorlds(RestWorlds, Evidence, NewSatWorlds, SatWorldsAcc)
	;
			\+satisfies(W,Evidence),
			satWorlds(RestWorlds, Evidence, SatisfyingWorlds, SatWorldsAcc)
	).

/*
*	Check if the world satisfies the evidence
*/
satisfies(World, Ev):-
	member(World,Ev).



/****************************
*****************************
*****************************
New Belief State Functions
*****************************
*****************************
****************************/


/*
*	Calculate the general formula for SME
*	Belief state is structured as a list of tuples (world, probability)
*/
calc_bs_general(_, [], _, _, _, _, New_BS, New_BS).

calc_bs_general(Belief_State, [(World,_)|RestWorlds], Evidence, Method, Gamma, B_Alpha, New_Worlds, New_Worlds_Acc):-
	calc_perc_world_general(Belief_State, Belief_State, World, Evidence, Method, Gamma, B_Alpha, 0, NewP),
	append(New_Worlds, [(World,NewP)], Output),
	calc_bs_general(Belief_State, RestWorlds, Evidence, Method, Gamma, B_Alpha, Output, New_Worlds_Acc).

/*
*	Calculate the percentage of this world in the new Belief State
*/
calc_perc_world_general(_, [], _, _, _, Gamma, _, StartP, NewP):-
	NewP is (Gamma ** (-1)) * StartP.
calc_perc_world_general(Belief_State, [(OtherWorld,ProbOtherW)|RestWorlds], CurrentWorld, Evidence, Method, Gamma, B_Alpha, StartP, P_Acc):-
	satisfies(CurrentWorld, Evidence),
	calculate_sigma(CurrentWorld, OtherWorld, B_Alpha, Method, Evidence, Sigma),
	NewStartP is StartP + (Sigma * ProbOtherW),
	calc_perc_world_general(Belief_State, RestWorlds, CurrentWorld, Evidence, Method, Gamma, B_Alpha, NewStartP, P_Acc).

calc_perc_world_general(_, _, _, _, _, _, _, _, 0).


/****************************
*****************************
*****************************
Run The program
*****************************
*****************************
****************************/

/*
Call the mainin function
*/
query(main()).