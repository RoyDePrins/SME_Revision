/*
*	This version is the "Feature" version. 
*	Meaning that the probability of each feature must be given,
*	the corresponding probabilities of the worlds will be automatically derived assuming all features are independent.
*	Also, the evidence must be given as a tuple containing the feature and the assigned value (0 or 1).
*/
:- use_module(library(lists)).
:- use_module(library(string)).
:- use_module(library(apply)).


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
	% Feature_Probs: 		[probability1, p2, ..., pn]
	Feature_Probs = [5/10,4/10,8/10],
	% Evidence_Probs:		[(featureNb, value), (f,v), ... (f,v)]
	Evidence_Probs = [(2,1)],
	% The used SME Revision method: {'SWITCH', 'THETA'}
	Method = 'SWITCH',	
	% Get a list containing all worlds in binary representation
	getAllBinaryWorlds(Feature_Probs, Binary_Worlds),
	% Make a belief state of the binary world list
	make_BS_From_Features(Binary_Worlds, Feature_Probs, Belief_State),
	% Make a list of evidence from the list of features and assigned values
	make_Evidence_From_Features(Binary_Worlds, Evidence_Probs, Evidence),
	% Perform SME on the given Belief State
	calculate_sme(Belief_State, Evidence, Method, New_Belief_State).


/****************************
*****************************
*****************************
Helper Functions
*****************************
*****************************
****************************/


/*
*	Calculate the length of a string (not buildIn in ProbLog)
*/
string_length(Str,Length):-
	str2lst(Str,StrList),
	calc_string_length(StrList,0,Length).

% '-2' necessary because it counts both quotes around the string as well.
calc_string_length([],L,Output):-
	Output is L - 2.

calc_string_length([_|RestString],Start, LengthAcc):-
	NewStart is Start + 1,
	calc_string_length(RestString,NewStart,LengthAcc).


/****************************
*****************************
*****************************
Get Binary Worlds
*****************************
*****************************
****************************/


/*
*	Get a list of all possible worlds in a binary representation
*/
getAllBinaryWorlds(Feature_Probs, Binary_Worlds):-
	length(Feature_Probs, Feature_Amount),
	% The amount of worlds
	TotalWorlds is 2**Feature_Amount,
	% Get the 'Names' of the worlds (= Index in the list)
	getWorlds(TotalWorlds, [], WorldList),
	% Transform the names to binary
	transformToBinaryWorlds(Feature_Amount, WorldList, [], Binary_Worlds).


/*
*	Get a list [0,1,...,TotalWorlds-1]
*/
getWorlds(0, WorldList, WorldList).
getWorlds(TotalWorlds, WorldList, WorldListAcc):-
	TotalWorlds > 0,
	NewTotalWorlds is TotalWorlds - 1,
	append([NewTotalWorlds], WorldList, NewWorldList),
	getWorlds(NewTotalWorlds, NewWorldList, WorldListAcc).


/*
*	Transform a list of integers to a Belief State of (Binary number, 1)-tuples
*/
transformToBinaryWorlds(_,[], BinaryWorldList, BinaryWorldList).
transformToBinaryWorlds(WorldLength, [Number|Rest], BinaryWorldList, BinaryWorldListAcc):-
	dec_to_bin(Number, BinaryShort),
	makeCorrectLength(WorldLength, BinaryShort, BinaryCorrect),
	append(BinaryWorldList, [BinaryCorrect], NewBinaryList),
	transformToBinaryWorlds(WorldLength, Rest, NewBinaryList, BinaryWorldListAcc).


/*
*	Make the given Binary number the correct length,
*	so all worlds in the Belief State have the same length
*/
makeCorrectLength(WorldLength, BinaryShort, BinaryCorrect):-
	appendZerosToString(BinaryShort,WorldLength, BinaryCorrect).

appendZerosToString(String, WorldLength, NewString):-
	string_length(String,StringLength),
 	Difference is WorldLength - StringLength,
 	addZerosString(Difference, String, NewString).

/*
*	Add zeros to the front of the given string
*/
addZerosString(0,String, String).
addZerosString(Difference, String, StringAcc):-
	Difference > 0,
 	concat(['0', String], NewString), 	
 	NewDifference is Difference - 1,
 	addZerosString(NewDifference, NewString, StringAcc).

/*
*	Transform the given integer to its equivalent binary representation
*/
dec_to_bin(0,'0').
dec_to_bin(1,'1').
dec_to_bin(Number, BinaryNumber):-
	Number > 1,
	X is Number mod 2,
	Y is Number//2,
	dec_to_bin(Y,B1),
	concat([B1, X], BinaryNumber).


/****************************
*****************************
*****************************
Transformation Features --> Worlds
*****************************
*****************************
****************************/

/*
*	Transform the given features into a Belief State
*/
make_BS_From_Features(Binary_Worlds, Feature_Probs, Belief_State):-
	transformToBinaryBS(Binary_Worlds, [], Belief_State_Ones), %Transform the binary world list into a Belief State with all Prob=1
	getCorrectBeliefState(Belief_State_Ones, 0, Feature_Probs, Belief_State). %Fill in the correct probs for the worlds, by means of the feature probs



/*
*	Transform a list of binary worlds into a BS of these worlds with all probabilities equal to 1.
*/
transformToBinaryBS([],BS_Ones, BS_Ones).
transformToBinaryBS([B_World|RestWorlds], BS_Ones_Acc, BS_Ones):-
	append(BS_Ones_Acc, [(B_World,1)], New_BS_Ones_Acc),
	transformToBinaryBS(RestWorlds, New_BS_Ones_Acc, BS_Ones).


/*
*	Calculate the probability for each world
*/
getCorrectBeliefState(BS, _, [], BS).
getCorrectBeliefState(Initial_Belief_State, FeatureNb, [P|RestProbs], BSAcc):-
	assignProbToWorlds(Initial_Belief_State, FeatureNb, P, [], NewBS),
	NewFeatureNB is FeatureNb + 1,
	getCorrectBeliefState(NewBS, NewFeatureNB, RestProbs, BSAcc).

/*
*	Assign the probability of the given feature (at featureNb) to all worlds in the list
*/
assignProbToWorlds([], _, _, BS, BS).
assignProbToWorlds([(World,WorldProb)|RestWorlds], FeatureNb, P, BS, NewBSAcc):-
	isOneOrZero(World, FeatureNb, OneZero),
	(		
			OneZero == 'ONE',
			Product is P
	;
			OneZero == 'ZERO',
			Product is 1 - P
	),
	NewWorldProb is WorldProb * Product,
	append(BS, [(World,NewWorldProb)],NewBS),
	assignProbToWorlds(RestWorlds, FeatureNb, P, NewBS, NewBSAcc).

/*
*	Check if the character at 'FeatureNB' is a one or a zero
*/
isOneOrZero(World, FeatureNb, OneZero):-
	getValueOfFeature(World,FeatureNb, Value),
	(
			Value == '1',
			OneZero = 'ONE'
	;
			Value == '0',
			OneZero = 'ZERO'
	).

/*
*	Get the value of the given feature in the given world
*/
getValueOfFeature(World,FeatureNb, Value):-
	str2lst(World, WorldList),
	Index is FeatureNb + 1,
	nth0(Index,WorldList,Value).




/****************************
*****************************
*****************************
Transformation Evidence_Features --> Evidence
*****************************
*****************************
****************************/

/*
*	Transform the evidence that is given as a list of tuples with the feature and the assigned value (FeatureNB, Value)
*	to a list containing every world that satisfies this evidence.
*	ex: (1,0) means that feature 1 needs to have a value of 0 (= is not present)
*/
make_Evidence_From_Features(Binary_Worlds, Evidence_Probs, Evidence):-
	include(belongsToEvidence(Evidence_Probs), Binary_Worlds, Evidence).

/*
*	Check if the given world is consistent with the Evidence_Probs
* 	explanation of NewFeatureNb: First feature has index 0, 2nd has index 1, ...
*/
belongsToEvidence([], _).	
belongsToEvidence([(FeatureNb, Value)|OtherFeatures], World):-
	NewFeatureNB is FeatureNb - 1,
	isOneOrZero(World, NewFeatureNB, OneZero),
	(
			Value == 1,
			ValueString = 'ONE'
	;
			Value == 0,
			ValueString = 'ZERO'

	),
	(
			OneZero == ValueString,
			belongsToEvidence(OtherFeatures, World)
	;
			OneZero \== ValueString,
			false
	).


/*
*	Check if the character at 'FeatureNB' is a one or a zero
*/
isOneOrZero(World, FeatureNb, OneZero):-
	getValueOfFeature(World,FeatureNb, Value),
	(
			Value == '1',
			OneZero = 'ONE'
	;
			Value == '0',
			OneZero = 'ZERO'
	).


/*
*	Get the value of the given feature in the given world
*/
getValueOfFeature(World,FeatureNb, Value):-
	str2lst(World, WorldList),
	Index is FeatureNb + 1,
	nth0(Index,WorldList,Value).


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