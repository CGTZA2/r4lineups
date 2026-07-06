# Functional Size with Bootstrapped Confidence Intervals

This function is a master function, calling other functions it needs,
and reporting results in some detail

## Usage

``` r
func_size_report(lineup_vec, target_pos, k, R)
```

## Arguments

- lineup_vec:

  A numeric vector of lineup choices

- target_pos:

  A scalar, representing target position in lineup. Must be declared by
  user

- k:

  Number of members in lineup. Must be specified by user (scalar).

- R:

  Number of bootstrap samples. Defaults to 1000

## Details

Function depends on functions from package 'boot'

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
#Data:
lineup_vec <- round(runif(100, 1, 6))
target_pos <- 3

#Call:
x <- func_size_report(lineup_vec, target_pos, 6)
#> Functional size of lineup is  5.556
#> Confidence intervals [95%]
#> Normal Theory 2.573 8.041
#> Bootstrap: percentile (R = 1000) 3.846 9.091
#> Bootstrap: bias-corrected (R = 1000) 3.704 8.333
x <- func_size_report(lineup_vec, 3, 6)
#> Functional size of lineup is  5.556
#> Confidence intervals [95%]
#> Normal Theory 2.498 8.031
#> Bootstrap: percentile (R = 1000) 3.846 9.091
#> Bootstrap: bias-corrected (R = 1000) 3.571 8.333
```
