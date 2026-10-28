# Master function: Homogeneity of diagnosticity ratio

This function provides assesses the homogeneity of the diagnosticity
ratio of k lineup pairs.

## Usage

``` r
homog_diag(lineup_pres_list, lineup_abs_list, pos_list, k, conf.level = 0.95)
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

- conf.level:

  Confidence level for the per-pair intervals, and the complement of the
  level at which heterogeneity is declared. Defaults to 0.95.

## Value

Invisibly, a list with the pooled (precision-weighted) diagnosticity
ratio, the chi-square statistic and its degrees of freedom and p-value,
a `heterogeneous` flag, and a `parameters` data frame giving the cell
counts plus \\d\\, \\\ln d\\, its variance, its weight and a confidence
interval for each lineup pair. The same information is printed.

## Details

Master function for assessing homogeneity of diagnosticity ratio for k
independent lineups.

The chi-square is Cochran's Q, \\\sum w_i (\ln d_i - \ln \bar{d})^2\\,
so it measures the *spread* of the k ratios around their pooled value.
It answers a different question from a confidence interval on the pooled
ratio, which asks whether the average ratio differs from 1. The two can
disagree: ratios that differ in direction (say 6, 1 and 0.44) give a
very large Q while averaging to roughly 1. When Q is significant the
ratios do not share a common value, the pooled estimate should not be
reported as a single summary, and the pairs should be interpreted
separately. This function prints a note to that effect and records it in
the `heterogeneous` element.

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
#> Diagnosticity ratio for each lineup pair:
#>  pair     d       95% CI
#>     1 1.000 [0.54, 1.84]
#>     2 1.000 [0.52, 1.94]
#>     3 1.000 [0.34, 2.93]
#> 
#> Mean diagnosticity ratio: 1
#> Chi-square estimate (q): 0
#> Sig: 1
#> 
#> Note: no evidence of heterogeneity (p >= 0.05), so the ratios are
#> consistent with a common value and the pooled mean is a reasonable summary.
```
