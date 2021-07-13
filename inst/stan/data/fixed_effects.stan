// matrix of fixed effects used to fit the durations
// has [clearN, fixedN] dimensions
int<lower=1> fixedN;
matrix[clearN, fixedN] fixed_effect;