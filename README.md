# Cumulative History Analaysis For Bistable Perception Timeseries

Estimates cumulative history for time-series for continuously viewed bistable perceptual rivalry displays. Computes cumulative history via a homogeneous first order differential process. I.e., it assumes exponential growth/decay of the history as a function time and perceptually dominant state. Supports Gamma, log normal, and normal distribution families. A package to compute a cumulative history for time-series of perceptual dominance in bistable displays.

## Installation

```r
library("devtools")
install_github("alexander-pastukhov/bistablehistory",dependencies=TRUE)
```

**Please note that the package is still under development!** It is almost feature-complete, but the documentation is still lacking. It is coming shortly with multiple vignettes on the cumulative history computation and meaning of individual parameters, use of fixed and random effects, use of different distributions, etc. If you run into a mistake or missing a feature, please raise an issue!

## Usage

The main function is `fit_cumhist` that takes a data frame with time-series as a first argument. Minimally, you need to specify `state` --- string with the column name that encodes perceptually dominant state --- and either `duration` (column name with duration of individual dominance phases) or `onset` (column name with onset times of individual dominance phases). Thus, for a simplest case of a single subject and single run/block measurement with all defaults (gamma distribution, fitted cumulative history time constant but fixed mixed state value and history mixing proportion) the call would be
```r
library(bistablehistory)
data(br_singleblock)
gamma_fit <- fit_cumhist(br_singleblock,
                        state="State",
                        duration="Duration")
```
or, equivalently
```r
library(bistablehistory)
data(br_singleblock)
gamma_fit <- fit_cumhist(br_singleblock,
                        state="State",
                        onset="Time")
```

Now you can look at the fitted value for history time constant via
```r
history_tau(gamma_fit)
```

and main effect of history for both parameters of gamma distribution
```r
coef(gamma_fit)
```

