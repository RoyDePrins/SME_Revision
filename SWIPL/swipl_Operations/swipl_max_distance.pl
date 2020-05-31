:- module(swipl_max_distance, [max_distance/3]).


/*
*	Calculate the maximal distance between the given worlds 
*	by using the given distance measure.
*/
max_distance(World, 'Hamming', Max_Dist):-
	string_to_list(World,WList),
	getMaxDistance_Hamming(WList, Max_Dist).

max_distance(_, 'Jaccard', Max_Dist):-
	getMaxDistance_Jaccard(Max_Dist).

max_distance(World, 'Euclidean', Max_Dist):-
	string_to_list(World,WList),
	getMaxDistance_Euclidean(WList, Max_Dist).

max_distance(_, 'Ochiai', Max_Dist):-
	getMaxDistance_Ochiai(Max_Dist).

max_distance(_, 'SMC', Max_Dist):-
	getMaxDistance_SMC(Max_Dist).

max_distance(_, Distance_Method, _):-
	write('Error in Distance method, the given method is: '),
	write(Distance_Method),
	nl,
	false.
/*
*	Calculate the maximal Hamming Distance
*/
getMaxDistance_Hamming(WList, Max_Dist):-
	getLength(WList, 0, Max_Dist).

/*
*	Calculate the maximal Jaccard Distance
*/
getMaxDistance_Jaccard(Max_Dist):-
	Max_Dist = 1.

/*
*	Calculate the maximal Euclidean Distance
*/
getMaxDistance_Euclidean(WList, Max_Dist):-
	getLength(WList, 0, N),
	Max_Dist = sqrt(N).

/*
*	Calculate the maximal Ochiai-Otsuka Distance
*/
getMaxDistance_Ochiai(Max_Dist):-
	Max_Dist = 1.

/*
*	Calculate the maximal SMC Distance
*/
getMaxDistance_SMC(Max_Dist):-
	Max_Dist = 1.


/*
*	Calculate the length of a given world
*/
getLength([],Length,Length).
getLength([_|Rest],Length,LengthAcc):-
	NewLength is Length + 1,
	getLength(Rest,NewLength,LengthAcc).
