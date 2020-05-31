:- module(swipl_getBinaryWorlds, [getAllBinaryWorlds/2]).



/*
*	Get a list of all possible worlds in a binary representation
*/
getAllBinaryWorlds(Feature_Probs, Binary_Worlds):-
	length(Feature_Probs, Feature_Amount),
	%The amount of worlds
	TotalWorlds is 2**Feature_Amount,
	%Get the 'Names' of the worlds = their number in the list
	getWorlds(TotalWorlds, [], WorldList),
	%Transform the names to binary
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

/*
*	Add zeros to a string
*/
appendZerosToString(String, WorldLength, NewString):-
	atom_length(String,StringLength),
 	Difference is WorldLength - StringLength,
 	addZerosString(Difference, String, NewString).

/*
*	Actually add zeros to the front of the given string
*/
 addZerosString(0, String, String).
 addZerosString(Difference, String, StringAcc):-
 	Difference > 0,
 	atom_concat('0', String, NewString),
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
	atom_concat(B1, X, BinaryNumber).
