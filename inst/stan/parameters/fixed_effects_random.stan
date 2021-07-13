// Parameters for the random slopes of the fixed terms
matrix<lower=0>[lmN, fixedN] bFixed_sigma;
row_vector[fixedN] bFixed_random[lmN, random_levelsN];
