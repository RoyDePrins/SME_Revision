:- module(swipl_distance, [distance/4]).
:- use_module(swipl_operations/swipl_max_distance).


/*
*	Calculate the distance between the given worlds 
*	by using the given distance measure.
*/
distance(World1, World2, Distance_Method, Dist):-
	string_to_list(World1,W1List),
	string_to_list(World2,W2List),
	% Calculates the 2x2 contingency table of the worlds
	calculate_coefficients(W1List, W2List, 0, 0, 0, 0, A, B, C, D),
	calculate_distance(World1, A, B, C, D, Distance_Method, Dist).


calculate_distance(_, _, B, C, _, 'Hamming', Dist):-
	getDistance_Hamming(B,C, Dist).

calculate_distance(World1, A, B, C, _, 'Jaccard', Dist):-
	getDistance_Jaccard(World1, A, B, C, Dist).

calculate_distance(World1, A, B, C, _, 'Ochiai', Dist):-
	getDistance_Ochiai(World1, A, B, C, Dist).

calculate_distance(_, _, B, C, _, 'Euclidean', Dist):-
	getDistance_Euclidean(B, C, Dist).

calculate_distance(_, A, B, C, D, 'SMC', Dist):-
	getDistance_SMC(A, B, C, D, Dist).

calculate_distance(_, _, _, _, _, Distance_Method, _):-
	write('Error in Distance method, the given method is: '),
	write(Distance_Method),
	nl,
	false.




/*
*	Calculate the coefficients A,B,C and D of the contingency table
*/
calculate_coefficients([],[], A, B, C, D, A, B, C, D).

% Both current feature values are "1"	--> Add to A
calculate_coefficients([49|T1],[49|T2], A, B, C, D, A_Acc, B_Acc, C_Acc, D_Acc):-
	NewA is A + 1,
	calculate_coefficients(T1,T2, NewA, B, C, D, A_Acc, B_Acc, C_Acc, D_Acc).

% Value of current feature of world1 is "0" and of world2 is "1"	--> Add to B
calculate_coefficients([48|T1],[49|T2], A, B, C, D, A_Acc, B_Acc, C_Acc, D_Acc):-
	NewB is B + 1,
	calculate_coefficients(T1,T2, A, NewB, C, D, A_Acc, B_Acc, C_Acc, D_Acc).

% Value of current feature of world1 is "1" and of world2 is "0"	--> Add to C
calculate_coefficients([49|T1],[48|T2], A, B, C, D, A_Acc, B_Acc, C_Acc, D_Acc):-
	NewC is C + 1,
	calculate_coefficients(T1,T2, A, B, NewC, D, A_Acc, B_Acc, C_Acc, D_Acc).

% Both current feature values are "0"		--> Add to D
calculate_coefficients([48|T1],[48|T2], A, B, C, D, A_Acc, B_Acc, C_Acc, D_Acc):-
	NewD is D + 1,
	calculate_coefficients(T1,T2, A, B, C, NewD, A_Acc, B_Acc, C_Acc, D_Acc).

/*
*	Calculate the Hamming Distance
*/
getDistance_Hamming(B,C, Dist):-
	Dist is B+C.

/*
*	Calculate the Jaccard Distance
*/
getDistance_Jaccard(World, A, B, C, Dist):-
	Numerator is A,
	Denumerator is A+B+C,
	(Denumerator =:= 0 -> 
		max_distance(World, 'Jaccard', Dist)
	;
		Dist is 1 - (Numerator / Denumerator)
	).


/*
*	Calculate the Ochiai-Otsuka Distance
*/
getDistance_Ochiai(World, A, B, C, Dist):-
	Numerator is A, 
	Denumerator is sqrt((A+B)*(A+C)),
	(Denumerator =:= 0 ->
		max_distance(World, 'Ochiai', OOD)
	;
		OOD is 1 - (Numerator / Denumerator)
	),
	Num is 2*acos(OOD),
	Dist is 1 - (Num/pi).


/*
*	Calculate the Euclidean Distance
*/
getDistance_Euclidean(B, C, Dist):-
	Unequal is B+C,
	Dist is sqrt(Unequal).

/*
*	Calculate the SMC Distance
*/
getDistance_SMC(A, B, C, D, Dist):-
	Numerator is B+C,
	Denumerator is A+B+C+D,
	Dist is (Numerator / Denumerator).