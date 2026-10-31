# Comparing Effective Size: Base function for bootstrapping

A row-resampling statistic for two columns of lineup choices.

## Usage

``` r
compare_eff_sizes.boot(linedf, d)
```

## Arguments

- linedf:

  A dataframe of lineup data. Must consist of 2 columns, each containing
  data for 2 independent lineups

- d:

  Indices for bootstrap sample. Argument used by boot function to select
  samples for bootstrapping

## Details

This statistic applies the same row indices to both columns. Using it
with an ordinary row bootstrap therefore resamples paired observations.
For independent lineup samples use
[`effsize_compare`](https://cgtza2.github.io/r4lineups/reference/effsize_compare.md),
which resamples each sample independently. Bootstrap intervals remain
approximations; inclusion or exclusion of zero does not establish
calibration in small or nearly uniform samples.

## References

Davison, A.C. & Hinkley, D.V. (1997). *Bootstrap methods and their
application*. Cambridge University Press.

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
of lineup size and lineup bias. *Applied Cognitive Psychology, 13*,
S9-S26.

Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
empirically assessing the fairness of a lineup. *Law and Human Behavior,
3*(4), 285-293.

## Examples

``` r
# Two independent lineups of 6 members, 50 mock witnesses each
set.seed(1)
linedf <- data.frame(lineup1 = sample(1:6, 50, replace = TRUE),
                     lineup2 = sample(1:6, 50, replace = TRUE))
# Difference in effective size for the observed data
compare_eff_sizes.boot(linedf, seq_len(nrow(linedf)))
#> [1] -0.07855048
# Resample the independent lineups separately
effsize_compare(linedf, R = 100)
#> 
#> The two Effective sizes are  5.681818   5.760369
#> If the interval includes 0, ns at p = .05
#> Confidence intervals of difference [95%]
#> Normal Theory -1.086 0.941
#> Bootstrap: percentile (R = 100) -1.355 0.949
#> Bootstrap: bias-corrected (R = 100) -1.281 0.964
```
