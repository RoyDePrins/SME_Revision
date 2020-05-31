:- module(swipl_SME_calculation, [calculate_sme/6]).
:- use_module(swipl_operations/swipl_sigma).


/****************************
*****************************
*****************************
		Main functions
*****************************
*****************************
****************************/


/*
*	Perform SME in the given belief state
*/
calculate_sme(Belief_State, Evidence, SME_Method, Sigma_Method, Distance_Method, New_Belief_State):-
	%Check if the given belief state is an actual belief state (Sum probs = 1)
	checkProbabilitySum(Belief_State),
	% Calculate B(alpha)
	calc_b_alpha(Belief_State, Evidence, B_Alpha),
	( 	SME_Method == 'SWITCH' ->
			calculate_sme_SWITCH(Belief_State, Evidence, Sigma_Method, Distance_Method, B_Alpha, New_Belief_State)
	; (	SME_Method == 'THETA' ->
			calculate_sme_THETA(Belief_State, Evidence, Sigma_Method, Distance_Method, B_Alpha, New_Belief_State)
	; (	SME_Method == 'THETA_OUT' ->
			calculate_sme_THETA_outside(Belief_State, Evidence, Sigma_Method, Distance_Method, B_Alpha, New_Belief_State)
	;
			write('Error in SME_method, the current method is: '),
			write(SME_Method),
			nl,
			false
	))),
	checkProbabilitySum(New_Belief_State),
	write(""),
	nl.

/*
*	The switch version of SME can be used with sigma_sh or sigma_GRBF and is ALWAYS combined with sigma_bc
*/
calculate_sme_SWITCH(Belief_State, Evidence, Sigma_Method, Distance_Method, B_Alpha, New_Belief_State):-
	(	 B_Alpha > 0 ->
			New_Sigma_Method = 'BC' 
	; (	(Sigma_Method == 'SH', B_Alpha =:= 0) ->
			New_Sigma_Method = 'SH'
	; ( (Sigma_Method == 'GRBF', B_Alpha =:= 0 ) ->
			New_Sigma_Method = 'GRBF'
	; 		
			write('Error in Sigma_method. Only "SH" and "GRBF" can be used in combination with "BC", the current method is: '),
			write(Sigma_Method),
			nl,
			false
	))),
	gamma_general(Belief_State, Evidence, New_Sigma_Method, Distance_Method, B_Alpha, Gamma),
	calc_bs_general(Belief_State, Belief_State, Evidence, New_Sigma_Method, Distance_Method, Gamma, B_Alpha, [], New_Belief_State).


/*
*	The graceful change version of SME can be used with sigma_sh or sigma_GRBF and is ALWAYS combined with sigma_bc
*/
calculate_sme_THETA(Belief_State, Evidence, Sigma_Method, Distance_Method, B_Alpha, New_Belief_State):-
	(	Sigma_Method == 'SH' ->
			New_Sigma_Method = 'THETA_SH' 
	; (	Sigma_Method == 'GRBF' ->
			New_Sigma_Method = 'THETA_GRBF' 
	; 		
			write('Error in Sigma_method. Only "SH" and "GRBF" can be used in graceful change with "BC", the current method is: '),
			write(Sigma_Method),
			nl,
			false
	)),
	gamma_general(Belief_State, Evidence, New_Sigma_Method, Distance_Method, B_Alpha, Gamma),
	calc_bs_general(Belief_State, Belief_State, Evidence, New_Sigma_Method, Distance_Method, Gamma, B_Alpha, [], New_Belief_State).



/*
*	The graceful change version of SME can be used with sigma_sh or sigma_GRBF and is ALWAYS combined with sigma_bc
*	Note that sigma_bc can only be calculated when B(alpha)>0
*/
calculate_sme_THETA_outside(Belief_State, Evidence, Sigma_Method, Distance_Method, B_Alpha, New_Belief_State):-
	((Sigma_Method == 'SH'; Sigma_Method == 'GRBF') ->
			true
	; 		
			write('Error in Sigma_method. Only "SH" and "GRBF" can be used in graceful change outside, the current method is: '),
			write(Sigma_Method),
			nl,
			false
	),
	gamma_general(Belief_State, Evidence, Sigma_Method, Distance_Method, B_Alpha, Gamma_SH),
	calc_bs_general(Belief_State, Belief_State, Evidence, Sigma_Method, Distance_Method, Gamma_SH, B_Alpha, [], New_Belief_State_Sigma),
	(B_Alpha > 0 ->
		gamma_general(Belief_State, Evidence, 'BC', Distance_Method, B_Alpha, Gamma_BC),
		calc_bs_general(Belief_State, Belief_State, Evidence, 'BC', Distance_Method, Gamma_BC, B_Alpha, [], New_Belief_State_BC),
		combine_Belief_States(New_Belief_State_BC, New_Belief_State_Sigma, B_Alpha, [], New_Belief_State)
	;	
		New_Belief_State = New_Belief_State_Sigma
	).

/*
*	Combine two belief states in relation to B_Alpha
*/
combine_Belief_States([], [], _, New_Belief_State, New_Belief_State).
combine_Belief_States([(World,Prob_BC)|Other_BS_BC], [(World,Prob_Sigma)|Other_BS_Sigma], B_Alpha, New_Belief_State, New_BS_Acc):-
	New_Prob is B_Alpha * Prob_BC + (1 - B_Alpha) * Prob_Sigma,
	append(New_Belief_State, [(World,New_Prob)], Output),
	combine_Belief_States(Other_BS_BC, Other_BS_Sigma, B_Alpha, Output, New_BS_Acc).


/****************************
*****************************
*****************************
Checks + useful functions
*****************************
*****************************
****************************/



/*
*	Check if the Sum of all probabilities in the given Belief Startte add to 1
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
*	TODO delete the 'write'-part
*/
calc_b_alpha(Belief_State, Evidence, B_Alpha) :- 
	getSatisfyingWorlds(Belief_State, Evidence, SatWorlds),
	getSumProbabilities(SatWorlds,0,B_Alpha).


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
gamma_general(Belief_State, Evidence, Sigma_Method, Distance_Method, B_Alpha, Gamma) :- 
	getSatisfyingWorlds(Belief_State, Evidence, SatisfyingWorlds),
	loopOverWorlds(Belief_State, SatisfyingWorlds, Evidence, Sigma_Method, Distance_Method, B_Alpha, 0, Gamma).

/*
*	Loop over the worlds and for each world calculate the calc_gamma_term_world and sum them all
*/
loopOverWorlds(_,[], _, _, _, _, Gamma, Gamma).
loopOverWorlds(Belief_state,[(W,_)|Tail], Evidence, Sigma_Method, Distance_Method, B_Alpha, Start, Gamma):-
	calc_gamma_term_World(Belief_state, Belief_state, Evidence, W, Sigma_Method, Distance_Method, B_Alpha, 0, Gamma_Term),
	NewStart is Start + Gamma_Term,
	loopOverWorlds(Belief_state, Tail, Evidence, Sigma_Method, Distance_Method, B_Alpha, NewStart, Gamma).

/*
*	Calculate the contribution of this world to Gamma as Gamma_Term
*/
calc_gamma_term_World(_,[], _, _, _, _, _, Gamma_Term, Gamma_Term).

calc_gamma_term_World(Belief_State,[(OtherWorld,ProbOtherW)|Tail], Evidence, CurrentWorld, Sigma_Method, Distance_Method, B_Alpha, Start, Gamma_Term):-
	calculate_sigma(CurrentWorld, OtherWorld, B_Alpha, Sigma_Method, Evidence, Distance_Method, Sigma),
	NewStart is Start + (ProbOtherW * Sigma),
	calc_gamma_term_World(Belief_State, Tail, Evidence, CurrentWorld, Sigma_Method, Distance_Method, B_Alpha, NewStart, Gamma_Term).


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
	(satisfies(W,Evidence) ->
			append([(W,P)], SatisfyingWorlds, NewSatWorlds),	
			satWorlds(RestWorlds, Evidence, NewSatWorlds, SatWorldsAcc)
	;
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
calc_bs_general(_, [], _, _, _, _, _, New_BS, New_BS).

calc_bs_general(Belief_State, [(World,_)|RestWorlds], Evidence, Sigma_Method, Distance_Method, Gamma, B_Alpha, New_Worlds, New_Worlds_Acc):-
	calc_perc_world_general(Belief_State, Belief_State, World, Evidence, Sigma_Method, Gamma, B_Alpha, Distance_Method, 0, NewP),
	append(New_Worlds, [(World,NewP)], Output),
	calc_bs_general(Belief_State, RestWorlds, Evidence, Sigma_Method, Distance_Method, Gamma, B_Alpha, Output, New_Worlds_Acc).

/*
*	Calculate the percentage of this world in the new Belief State
*/
calc_perc_world_general(_, [], _, _, _, Gamma, _, _, StartP, NewP):-
	NewP is (Gamma ** (-1)) * StartP.
calc_perc_world_general(Belief_State, [(OtherWorld,ProbOtherW)|RestWorlds], CurrentWorld, Evidence, Sigma_Method, Gamma, B_Alpha, Distance_Method, StartP, P_Acc):-
	satisfies(CurrentWorld, Evidence),
	calculate_sigma(CurrentWorld, OtherWorld, B_Alpha, Sigma_Method, Evidence, Distance_Method, Sigma),
	NewStartP is StartP + (Sigma * ProbOtherW),
	calc_perc_world_general(Belief_State, RestWorlds, CurrentWorld, Evidence, Sigma_Method, Gamma, B_Alpha, Distance_Method, NewStartP, P_Acc).

calc_perc_world_general(_, _, _, _, _, _, _, _, _, 0).
