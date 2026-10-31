# Functional Size with Bootstrapped Confidence Intervals

This function is a master function, calling other functions it needs,
and reporting results in some detail

## Usage

``` r
func_size_report(lineup_vec, target_pos, k, R = 1000)
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

## Value

Invisibly returns the functional-size estimate; the normal, percentile
and BCa bootstrap intervals; the exact interval; the number of
replications; the number of replications in which nobody chose the
target (`n_infinite`); and the bootstrap object, whose statistic is the
proportion choosing the target.

## Details

Functional size is the reciprocal of the proportion of mock witnesses
choosing the target, p. The bootstrap resamples p, which stays finite
when a resample contains no target choices, and the interval limits for
p are inverted. Percentile and BCa intervals respect monotone
transformations, so they are the intervals for functional size itself,
computed from the whole bootstrap distribution; a lower limit of 0 for p
gives an unbounded (`Inf`) upper limit. The normal interval is the
normal-theory interval for p, inverted. The exact interval inverts the
Clopper-Pearson interval for p
([`binom.test`](https://rdrr.io/r/stats/binom.test.html)). Near zero or
one, empirical bootstrap intervals can have severe undercoverage,
including a degenerate interval when the suspect was never chosen.
Prefer the exact interval in these cases; retaining infinite limits
alone does not guarantee nominal bootstrap coverage.

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
#> Functional size of lineup is 5.556
#> Confidence intervals [95%]
#> Normal Theory 3.936 9.623 
#> Bootstrap: percentile (R = 1000) 3.846 9.091 
#> Bootstrap: bias-corrected (R = 1000) 4.021 10 
#> Exact (Clopper-Pearson) 3.711 9.065 
x <- func_size_report(lineup_vec, 3, 6)
#> Functional size of lineup is 5.556
#> Confidence intervals [95%]
#> Normal Theory 3.894 9.684 
#> Bootstrap: percentile (R = 1000) 3.846 9.091 
#> Bootstrap: bias-corrected (R = 1000) 4 10 
#> Exact (Clopper-Pearson) 3.711 9.065 
```
