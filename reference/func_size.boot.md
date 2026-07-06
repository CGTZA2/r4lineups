# Bootstrapped Functional Size

This function is a base function for the bootstrapping that ensues to
compute bootstrapped confidence intervals for the estimate of functional
size

## Usage

``` r
func_size.boot(lineup_vec, d = d, target_pos)
```

## Arguments

- lineup_vec:

  A numeric vector of lineup choices

- d:

  Indices for bootstrap resampling

- target_pos:

  A scalar, representing position of target in lineup. Must be declared
  by user

## Details

Function is called bootstrap resampling using boot function (in package
'boot') This function is never called by the user - it is called in the
functional size master function

## References

Davison, A.C. & Hinkley, D.V. (1997). *Bootstrap methods and their
application*. Cambridge University Press.

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22*(2), 217-237.

Tredoux, C. (1999). Statistical considerations when determining measures
of lineup size and lineup bias. *Applied Cognitive Psychology*, 13,
S9-S26.

Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
empirically assessing the fairness of a lineup. *Law and Human Behavior,
3*(4), 285-293.

## See also

[`boot`](https://rdrr.io/pkg/boot/man/boot.html):
https://cran.r-project.org/web/packages/boot/boot.pdf

## Examples

``` r
# Lineup choices of 50 mock witnesses; target in position 3
set.seed(1)
lineup_vec <- sample(1:6, 50, replace = TRUE)
# Functional size for the observed data
func_size.boot(lineup_vec, seq_along(lineup_vec), target_pos = 3)
#> [1] Inf
# Bootstrap functional size
boot_fs <- boot::boot(lineup_vec, func_size.boot, R = 100, target_pos = 3)
boot::boot.ci(boot_fs, type = "perc")
#> Warning: extreme order statistics used as endpoints
#> BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#> Based on 11 bootstrap replicates
#> 
#> CALL : 
#> boot::boot.ci(boot.out = boot_fs, type = "perc")
#> 
#> Intervals : 
#> Level     Percentile     
#> 95%   ( 1,  1 )  
#> Calculations and Intervals on Original Scale
#> Warning : Percentile Intervals used Extreme Quantiles
#> Some percentile intervals may be unstable
```
