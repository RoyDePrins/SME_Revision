:- module(swipl_BS_from_features, [make_BS_From_Features/3]).


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
	% Transform the binary world list into a Belief State with all Prob=1
	transformToBinaryBS(Binary_Worlds, [], Belief_State_Ones),
	% Fill in the correct probs for the worlds, by means of the feature probs
	getCorrectBeliefState(Belief_State_Ones, 0, Feature_Probs, Belief_State).

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
*	Assign the probability of the given feature (at corresponding featureNb) to all worlds in the list
*/
assignProbToWorlds([], _, _, BS, BS).
assignProbToWorlds([(World,WorldProb)|RestWorlds], FeatureNb, P, BS, NewBSAcc):-
	isOneOrZero(World, FeatureNb, OneZero),
	(OneZero == 'ONE' ->
			Product is P
	;
			Product is 1 - P
	),
	NewWorldProb is WorldProb * Product,
	append(BS, [(World,NewWorldProb)],NewBS),
	assignProbToWorlds(RestWorlds, FeatureNb, P, NewBS, NewBSAcc).


/*
*	Check if the character at 'FeatureNB' is a one or a zero
*/
isOneOrZero(World, FeatureNb, OneZero):-
	% Get the character at position FeatureNb
	sub_atom(World, FeatureNb, 1, _, Char),
	(Char == '1' ->
			OneZero = 'ONE'
	;
			OneZero = 'ZERO'
	).