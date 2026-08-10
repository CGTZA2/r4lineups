# Bootstrapped Effective Size (Tredoux, 1998)

Base function for generating bootstrapped Effective Size (Tredoux, 1998)

## Usage

``` r
esize_T_boot(lineup_table, d)
```

## Arguments

- lineup_table:

  A vector of individual lineup choices. The historical argument name is
  retained for compatibility.

- d:

  Indices for bootstrap resampling

## Details

This is the statistic function passed to
[`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) for
bootstrapping Tredoux's E'. The data argument must contain individual
lineup choices; `d` resamples mock witnesses rather than count-table
cells. For a higher-level interface that accepts raw lineup vectors, see
[`esize_boot_dist`](https://cgtza2.github.io/r4lineups/reference/esize_boot_dist.md).

## References

Davison, A.C. & Hinkley, D.V. (1997). *Bootstrap methods and their
application*. Cambridge University Press.

Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
identification lineups. *Law and Human Behavior, 5*(4), 299-309.

Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
& M. P. Toglia (Eds.), Handbook of Eyewitness Psychology, Vol. 2: Memory
for people (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22(2)*, 217-237.

Tredoux, C. (1999). Statistical considerations when determining measures
of lineup size and lineup bias. *Applied Cognitive Psychology, 13*,
S9-S26.

Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
empirically assessing the fairness of a lineup. *Law and Human Behavior,
3*(4), 285-293.

## See also

[`boot`](https://rdrr.io/pkg/boot/man/boot.html):
https://cran.r-project.org/web/packages/boot/boot.pdf

## Examples

``` r
# Choices from 50 mock witnesses to a 6-member lineup
set.seed(1)
lineup_table <- sample(1:6, 50, replace = TRUE)
# E' for the observed data
esize_T_boot(lineup_table, seq_along(lineup_table))
#> [1] 5.681818
```
