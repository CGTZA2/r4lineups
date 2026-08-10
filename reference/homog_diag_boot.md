# Homogeneity of diagnosticity ratio with bootstrapped CIs

Function for computing bootstrapped estimates of homogeneity of
diagnosticity ratio

## Usage

``` r
homog_diag_boot(
  lineup_pres_list,
  lineup_abs_list,
  k,
  R = 100,
  pos_list = NULL,
  seed = NULL
)
```

## Arguments

- lineup_pres_list:

  A list containing k vectors of lineup choices for k lineups, in which
  the target was present

- lineup_abs_list:

  A list containing k vectors of lineup choices for k lineups, in which
  the target was absent

- k:

  Number of members in lineup. Must be specified by user (scalar).

- R:

  Number of bootstrap replications. Defaults to R = 100.

- pos_list:

  Suspect positions for each lineup pair, in the same format as
  [`diag_param()`](https://cgtza2.github.io/r4lineups/reference/diag_param.md).
  This is required; earlier releases attempted to infer suspect
  positions from observed choices, which cannot be done validly.

- seed:

  Optional integer seed for reproducible resampling.

## Value

Invisibly returns a list containing the observed mean diagnosticity,
chi-square statistic, percentile intervals, bootstrap draws, and `R`.

## Details

Computes bootstrapped diagnosticity ratio with chi-squared estimate,
significance level and confidence intervals for k lineup pairs

## References

Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
identification lineups. *Law and Human Behavior, 5*(4), 299-309.

Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
& M. P. Toglia (Eds.), *Handbook of Eyewitness Psychology, Vol. 2:
Memory for people* (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum
Associates.

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22*(2), 217-237.

Tredoux, C. (1999). Statistical considerations when determining measures
of lineup size and lineup bias. *Applied Cognitive Psychology*, 13,
S9-S26.

Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
empirically assessing the fairness of a lineup. *Law and Human Behavior,
3*(4), 285-293.

## Examples

``` r
#Target present data:
A <- rep(1:6, length.out = 100)
B <- rep(1:5, length.out = 70)
C <- rep(1:4, length.out = 20)
lineup_pres_list <- list(A, B, C)
rm(A, B, C)

#Target absent data:
A <- rep(6:1, length.out = 100)
B <- rep(5:1, length.out = 70)
C <- rep(4:1, length.out = 20)
lineup_abs_list <- list(A, B, C)
rm(A, B, C)

pos_list <- c(3, 2, 1)
k <- c(6, 5, 4)
homog_diag_boot(lineup_pres_list, lineup_abs_list, k, R = 20,
               pos_list = pos_list, seed = 1)
#> Mean diagnosticity ratio is 1 
#> Confidence intervals (percentile) 0.757 1.395 
#> Chi-squared estimate is 0 
#> Confidence intervals (percentile): 0.333 4.967 
```
