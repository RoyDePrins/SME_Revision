:- module(swipl_mse, [mse/3]).


/*
*	Calculate the mean square error between the probabilities of both given belief states
*/
mse([(W1,P1)|Rest_BS], New_BS, Dist):-
	get_total_worlds(W1, TotalWorlds),
	calc_mse([(W1,P1)|Rest_BS], New_BS, TotalWorlds, 0, Dist).

/*
*	Get the amount of worlds in the belief states
*/
get_total_worlds(World, TotalWorlds):-
	atom_length(World, WorldLength),
	TotalWorlds is 2 ** WorldLength.

/*
*	Calculate the average squared distance
*/
calc_mse([], [], TotalWorlds, Dist, TotalDist):-
	TotalDist is Dist/TotalWorlds.
calc_mse([(_,P1)|Rest_BS], [(_,P2)|Rest_New_BS], TotalWorlds, Dist, DistAcc):-
	NewDist = Dist + ((P1-P2)**2),
	calc_mse(Rest_BS, Rest_New_BS, TotalWorlds, NewDist, DistAcc).