# Bootstrapped lineup proportion

Base function for computing bootstrapped lineup proportion for a lineup
member

## Usage

``` r
lineup_prop_boot(lineup_vec, d, target_pos)
```

## Arguments

- lineup_vec:

  A numeric vector of lineup choices

- d:

  Indices for bootstrap sample. Argument used by boot function to select
  samples for bootstrapping

- target_pos:

  A scalar, representing target position in lineup. Must be declared by
  user

## Details

Function to call when bootstrap resampling using boot function

## References

Davison, A.C. & Hinkley, D.V. (1997). *Bootstrap methods and their
application*. Cambridge University Press.

Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
empirically assessing the fairness of a lineup. *Law and Human Behavior,
3*(4), 285-293.

## See also

[`boot`](https://rdrr.io/pkg/boot/man/boot.html):
https://cran.r-project.org/web/packages/boot/boot.pdf

## Examples

``` r
lineup_vec <- round(runif(100, 1, 6))

bootobject <- boot::boot(lineup_vec, lineup_prop_boot, target_pos = 3, R = 1000)
cis <- boot::boot.ci(bootobject, conf = 0.95, type = "all")
#> Warning: bootstrap variances needed for studentized intervals
```
