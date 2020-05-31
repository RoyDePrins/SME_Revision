:- module(swipl_sigma, [calculate_sigma/7]).
:- use_module(swipl_operations/swipl_distance).
:- use_module(swipl_operations/swipl_max_distance).

/*
*	Calculate the similarity between the given worlds
*	by using the given similarity measure
*/
calculate_sigma(CurrentWorld, OtherWorld, _, 'BC', _, _, Sigma):-
	sigma_bc(CurrentWorld, OtherWorld, Sigma).

calculate_sigma(CurrentWorld, OtherWorld, _, 'SH', Evidence, Distance_Method, Sigma):-
	sigma_sh(CurrentWorld, OtherWorld, Evidence, Distance_Method, Sigma).

calculate_sigma(CurrentWorld, OtherWorld, B_Alpha, 'THETA_SH', Evidence, Distance_Method, Sigma):-
	sigma_theta_SH(CurrentWorld, OtherWorld, B_Alpha, Evidence, Distance_Method, Sigma).

calculate_sigma(CurrentWorld, OtherWorld, _, 'GRBF', Evidence, Distance_Method, Sigma):-
	sigma_grbf(CurrentWorld, OtherWorld, Evidence, Distance_Method, Sigma).

calculate_sigma(CurrentWorld, OtherWorld, B_Alpha, 'THETA_GRBF', Evidence, Distance_Method, Sigma):-
	sigma_theta_GRBF(CurrentWorld, OtherWorld, B_Alpha, Evidence, Distance_Method, Sigma).

calculate_sigma(_, _, _, Sigma_Method, _, _, _):-
	write('Error in Sigma_method, the current method is: '),
	write(Sigma_Method),
	nl,
	false.


/*
*	Calculate the similartity when using Shepard
*/
% Both worlds are the same
sigma_sh(World1, World1, _, _, 1).

% Both worlds belong to the evidence
sigma_sh(World1, World2, Evidence, Distance_Method, Sigma):-
	satisfies(World1, Evidence),
	satisfies(World2, Evidence),
	distance(World1, World2, Distance_Method, Dist),
	Sigma is e ** (-Dist).

% One of the worlds does not satisfy the evidence
sigma_sh(World1, World2, _, Distance_Method, Sigma):-
	distance(World1, World2, Distance_Method, Dist),
	max_distance(World1, Distance_Method, Max_Dist),
	Power is -Dist - Max_Dist,
	Sigma is e ** Power.


/*
*	Calculate the similarity using Bayesian Conditioning
*/
%Both worlds are the same
sigma_bc(World1, World1, 1).
%Both worlds are different
sigma_bc(_, _, 0).

/*
*	Calculate the similarity using Theta and SH
*/
sigma_theta_SH(World1, World2, B_Alpha, Evidence, Distance_Method, Sigma):-
	sigma_bc(World1, World2, Sigm_bc),
	sigma_sh(World1, World2, Evidence, Distance_Method, Sigm_sh),
	Sigma is B_Alpha * Sigm_bc + (1 - B_Alpha) * Sigm_sh.

/*
*	Calculate the similarity using Theta and GRBF
*/
sigma_theta_GRBF(World1, World2, B_Alpha, Evidence, Distance_Method, Sigma):-
	sigma_bc(World1, World2, Sigm_bc),
	sigma_grbf(World1, World2, Evidence, Distance_Method, Sigm_sh),
	Sigma is B_Alpha * Sigm_bc + (1 - B_Alpha) * Sigm_sh.


/*
*	Calculate the similarity using an exponential Generalized Radial Basis Function
*	TODO: Give a reasonable value for Kappa
*/
% Both worlds are the same
sigma_grbf(World1, World1, _, _, 1).

% Both worlds satisfy the evidence
sigma_grbf(World1, World2, Evidence, Distance_Method, Sigma):-
	satisfies(World1, Evidence),
	satisfies(World2, Evidence),
	distance(World1, World2, Distance_Method, Dist),
	Kappa is 1,
	Numerator is -(Dist ** 2),
	Denumerator is 2 * (Kappa ** 2),
	Sigma is e ** (Numerator / Denumerator).

% One of the worlds does not satisfy the evidence
sigma_grbf(World1, World2, _, Distance_Method, Sigma):-
	distance(World1, World2, Distance_Method, Dist),
	max_distance(World1, Distance_Method, Max_Dist),
	Kappa is Max_Dist/2,
	Numerator is -(Dist ** 2) - Max_Dist,
	Denumerator is 2 * (Kappa ** 2),
	Sigma is e ** (Numerator / Denumerator).


/*
*	SME_SWITCH as a sigma-operator
*/
sigma_SWITCH(CurrentWorld, OtherWorld, B_Alpha, Evidence, Distance_Method, Sigma):-
	(B_Alpha > 0 ->
		sigma_bc(CurrentWorld, OtherWorld, Sigma)
	;
		sigma_sh(CurrentWorld, OtherWorld, Evidence, Distance_Method, Sigma)
	).



/*
*	Check if the world satisfies the evidence
*/
satisfies(World, Ev):-
	member(World,Ev).

