// We assume that there can be only one random effect
int<lower=1> random_levelsN; // number of levels in random FACTOR
int<lower=1, upper=random_levelsN> random_effect[rowsN]; // which participant the data belongs to for entire time series
int<lower=1, upper=random_levelsN> clear_random_effect[clearN]; // which participant the data belongs to just for the clear (fitted) durations
