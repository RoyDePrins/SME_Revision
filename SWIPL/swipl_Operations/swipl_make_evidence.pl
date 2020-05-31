:- module(swipl_make_evidence, [make_Evidence_From_Features/3]).

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
*/
belongsToEvidence([], _).
%Check if the value of the current feature is valid for this world
belongsToEvidence([(FeatureNb, Value)|OtherFeatures], World):-
	NewFeatureNB is FeatureNb - 1, %explanation: First feature has index 0, 2nd has index 1, ...
	isOneOrZero(World, NewFeatureNB, OneZero),
	(Value == 1 ->
			ValueString = 'ONE'
	;
			ValueString = 'ZERO'

	),
	(OneZero == ValueString ->
			belongsToEvidence(OtherFeatures, World)
	;
			false
	).



/*
* Check if the character at 'FeatureNB' is a one or a zero
*/
isOneOrZero(World, FeatureNb, OneZero):-
	%Get the character at position FeatureNb
	sub_atom(World, FeatureNb, 1, _, Char),
	(Char == '1' ->
			OneZero = 'ONE'
	;
			OneZero = 'ZERO'
	).