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

  Desired level of alpha. Defaults to 0.95. May be specified by user
  (scalar).

- R:

  Number of bootstrap replications. Defaults to 1000.

## Value

Returns a vector of bias corrected confidence intervals for lineup
proportion for each member in a lineup

## Details

Function that computes bootstrapped lineup proportion using 1000
bootstrap draws Calls 'boot function in 'boot' package

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
