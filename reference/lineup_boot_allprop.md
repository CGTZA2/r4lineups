# Confidence intervals for lineup proportion

Computes bootstrapped confidence intervals for lineup proportion

## Usage

``` r
lineup_boot_allprop(lineup_vec, k, conf = 0.95, R = 1000)
```

## Arguments

- lineup_vec:

  A numeric vector of lineup choices

- k:

  Number of targets in lineup. Must be specified by user (scalar).

- conf:

  Confidence level for the intervals, e.g. 0.95 for 95 percent
  intervals. Defaults to 0.95. May be specified by user (scalar).

- R:

  Number of bootstrap replications. Defaults to 1000.

## Value

A dataframe of `k` rows and two columns, `ci_low` and `ci_high`, holding
the bias-corrected and accelerated (BCa) confidence interval for the
proportion choosing each lineup member. Rows are named by lineup
position.

## Details

Computes a bootstrapped lineup proportion for each member in turn by
calling the `boot` function in the `boot` package. Where a BCa interval
cannot be computed, the percentile interval is used instead.

Note that the `k` intervals are each computed at the nominal `conf`
level, with **no** adjustment for the fact that `k` of them are
inspected together. Interpreting all of them at once inflates the chance
that at least one excludes the chance value by more than the nominal
amount. If a family-wise statement is wanted, raise the level for each
interval accordingly, e.g. `conf = 1 - 0.05 / k` for a Bonferroni-style
correction.

## References

Davison, A.C. & Hinkley, D.V. (1997). *Bootstrap methods and their
application*. Cambridge University Press.

Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
assessing the fairness of a lineup. *Law and Human Behavior, 3*(4),
285-293.

## See also

[`boot`](https://rdrr.io/pkg/boot/man/boot.html):
https://cran.r-project.org/web/packages/boot/boot.pdf

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1, 6))

#Call:
lineuprops_ci <- lineup_boot_allprop(lineup_vec, k= 6)
lineuprops_ci <- lineup_boot_allprop(lineup_vec, k= 6, conf = 0.975)
```
