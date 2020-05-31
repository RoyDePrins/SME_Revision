:- module(swipl_world, [apply_SME_Revision_world/6]).


:- use_module(swipl_Operations/swipl_SME_calculation).
/*
*	This version is the "World" version. 
*	Meaning that the probability of each world must be given.
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
*		- Belief State: 		[(world1,prob1),(w2,p2),...(wn,pn)]
*		- Evidence:				[world1, world2, ..., wn]
*		- SME_Method:			{'SWITCH', 'THETA', 'THETA_OUT'}
*		- Sigma_Method:			{'SH', 'GRBF'}
*		- Distance_Method:		{'Hamming', 'Jaccard', 'SMC', 'Ochiai', 'Euclidean'}
*		- New_Belief_State:		Any variable name starting with a capital letter
*/
apply_SME_Revision_world(Belief_State, Evidence, SME_Method,Sigma_Method, Distance_Method, New_Belief_State):-
	% Perform SME on the given Belief State
	calculate_sme(Belief_State, Evidence, SME_Method, Sigma_Method, Distance_Method, New_Belief_State).