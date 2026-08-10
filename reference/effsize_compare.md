# Master Function: Comparing Effective Size

Function for comparing effective size of two independent lineups
(Tredoux, 1998)

## Usage

``` r
effsize_compare(linedf, R = 1000)
```

## Arguments

- linedf:

  A dataframe of lineup data. Contains 2 columns, each of which hold
  data for 2 independent lineups

- R:

  Number of bootstrap replications.

## Value

Effective size, significance level, and confidence intervals (95 normal
theory, percentile & bias-corrected)

## Details

This function is a master function, calling other functions it needs,
and reporting results in some detail

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

## See also

[`boot`](https://rdrr.io/pkg/boot/man/boot.html):
https://cran.r-project.org/web/packages/boot/boot.pdf

## Examples

``` r
#Data:

lineup_vec1 <- round(runif(100, 1, 6))
lineup_vec2 <- round(runif(100, 1, 6))
linedf <- as.data.frame(cbind(lineup_vec1, lineup_vec2))

#Call:
x <- effsize_compare(linedf, R = 100)
#> 
#> The two Effective sizes are  5.847953   5.411255
#> If the interval includes 0, ns at p = .05
#> Confidence intervals of difference [95%]
#> Normal Theory -0.425 1.242
#> Bootstrap: percentile (R = 100) -0.386 1.281
#> Bootstrap: bias-corrected (R = 100) -0.395 1.279
```
