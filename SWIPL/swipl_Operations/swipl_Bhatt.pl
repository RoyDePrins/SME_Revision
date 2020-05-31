:- module(swipl_bhatt, [calculate_bhatt/3]).
:- use_module(swipl_probdist_distance).

/*
*	Calculate the Bhattacharyya-Divergence of both probability distributions
* 	Due to practicallity reasons we calculate the distance differently when BC equals zero,
*	because the Bhatt. Distance is not defined for this value.
*/
calculate_bhatt(BS, New_BS, Bhatt):-
	calculate_bhatt_coef(BS, New_BS, 0, Bhatt_Term),
	(Bhatt_Term =:= 0 ->
		write('The MSE is used instead of the Bhatt. distance.'),nl,
		mse(New_BS, BS, Bhatt)
	;
		Bhatt is -log(Bhatt_Term)	
	).

/*
*	Calculate the Bhattacharyya coefficient
*/
calculate_bhatt_coef([], [], Bhatt, Bhatt).
calculate_bhatt_coef([(_,P1)|Rest_BS], [(_,P2)|Rest_New_BS], Bhatt, Bhatt_Acc):-
	Newbhatt is Bhatt + sqrt(P1 * P2),
	calculate_bhatt_coef(Rest_BS, Rest_New_BS, Newbhatt, Bhatt_Acc). 