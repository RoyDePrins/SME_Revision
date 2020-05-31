:- module(swipl_feature, [apply_SME_Revision_feature/6]).

:- use_module(swipl_operations/swipl_SME_calculation).
:- use_module(swipl_operations/swipl_getBinaryWorlds).
:- use_module(swipl_operations/swipl_BS_from_features).
:- use_module(swipl_operations/swipl_make_evidence).



/*
*	This version is the "Feature" version. 
*	Meaning that the probability of each feature in the belief state
* 	and the probability of each feature in the evidence must be given.
*/

/****************************
*****************************
*****************************
Main Function
*****************************
*****************************
****************************/


/*
*	Main function to run the program.
*
*	The following values for the arguments are allowed:	
*		- Feature_Probs: 		[probability1, p2, ..., pn]
*		- Evidence_Probs:		[(featureNb, value), (f,v), ... (f,v)]
*		- SME_Method:			{'SWITCH', 'THETA', 'THETA_OUT'}
*		- Sigma_Method:			{'SH', 'GRBF'}
*		- Distance_Method:		{'Hamming', 'Jaccard', 'SMC', 'Ochiai', 'Euclidean'}
*		- New_Belief_State:		Any variable name starting with a capital letter
*/
apply_SME_Revision_feature(Feature_Probs, Evidence_Probs, SME_Method, Sigma_Method, Distance_Method, New_Belief_State):-
	% Calculate the Original Belief State and the List of worlds that satisfy the Evidence
	calculate_BS_and_Evidence(Feature_Probs, Evidence_Probs, Belief_State, Evidence),
	% Perform SME on the given Belief State
	calculate_sme(Belief_State, Evidence, SME_Method, Sigma_Method, Distance_Method, New_Belief_State).


/*
*	Calculate the Original Belief State and the List of Evidence
*/
calculate_BS_and_Evidence(Feature_Probs, Evidence_Probs, Belief_State, Evidence):-
	% Get a list of all the worlds in a binary representation
	getAllBinaryWorlds(Feature_Probs, Binary_Worlds),
	% Make this into a correct belief state
	make_BS_From_Features(Binary_Worlds, Feature_Probs, Belief_State),
	% Make this into a correct list of all the worlds that satisfy the evidence
	make_Evidence_From_Features(Binary_Worlds, Evidence_Probs, Evidence).
