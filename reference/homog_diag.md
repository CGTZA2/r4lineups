# Master function: Homogeneity of diagnosticity ratio

This function provides assesses the homogeneity of the diagnosticity
ratio of k lineup pairs.

## Usage

``` r
homog_diag(lineup_pres_list, lineup_abs_list, pos_list, k)
```

## Arguments

- lineup_pres_list:

  A list containing k vectors of lineup choices for k lineups, in which
  the target was present

- lineup_abs_list:

  A list containing k vectors of lineup choices for k lineups, in which
  the target was absent

- pos_list:

  Suspect positions for each lineup pair. See
  [`diag_param()`](https://cgtza2.github.io/r4lineups/reference/diag_param.md).

- k:

  A vector indexing number of members in each lineup pair (nominal
  size). Must be specified by user (scalar).

## Value

Computes diagnosticity ratio with chi-squared estimate and significance
level for k lineup pairs

## Details

Master function for assessing homogeneity of diagnosticity ratio for k
independent lineups.

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

# Suspect position for each TP/TA pair
pos_list <- c(3, 2, 1)

#Nominal size:
k <- c(6, 5, 4)

#Call:
homog_diag(lineup_pres_list, lineup_abs_list, pos_list, k)
#> Mean diagnosticity ratio: 1
#> Chi-square estimate (q): 0
#> Sig: 1
```
