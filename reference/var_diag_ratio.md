# Variance of diagnosticity ratio (Tredoux)

Computes the variance of the diagnosticity ratio for a lineup pair

## Usage

``` r
var_diag_ratio(lineup_pres, lineup_abs, pos_pres, pos_abs, k1, k2)
```

## Arguments

- lineup_pres:

  A numeric vector of lineup choices for a lineup in which the target
  was present

- lineup_abs:

  A numeric vector of lineup choices for a lineup in which the target
  was absent

- pos_pres:

  A scalar, representing target position in TP lineup. Must be declared
  by user

- pos_abs:

  A scalar, representing target position in TA lineup. Must be declared
  by user

- k1:

  Number of targets in TP lineup. Must be specified by user (scalar).

- k2:

  Number of targets in TA lineup. Must be specified by user (scalar).

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
#Data:
lineup_pres <- round(runif(100, 1, 6))
lineup_abs <- round(runif(100, 1, 6))
pos_pres <- 3
pos_abs <- 4

#Call:
var_d <- var_diag_ratio(lineup_pres, lineup_abs, pos_pres, pos_abs, 6, 6)
var_d <- var_diag_ratio(lineup_pres, lineup_abs, 3, 4, 6, 6)
```
