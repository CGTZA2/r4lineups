# Effective Size

Function for computing Effective Size

## Usage

``` r
esize_m(lineup_table, k, both = FALSE)
```

## Arguments

- lineup_table:

  A table of lineup choices.

- k:

  Number of members in the lineup. Must be specified by the user
  (scalar).

- both:

  Logical. Defaults to FALSE, returning Tredoux's adjusted effective
  size estimate only. If TRUE, both Malpass's (1981) original
  formulation and Malpass's adjusted version (Tredoux, 1998) are printed
  to the console.

## Value

Malpass's original & adjusted estimates of effective size

## Details

Reduces the size of a lineup from a (corrected) nominal starting value
by the degree to which members are, in sum, chosen below the level of
chance expectation.

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
#Data:
lineup_vec <- round(runif(100, 1, 6))

#Call:
esize_m(lineup_vec, 6, both = TRUE)
#> Effective size (Malpass, 1981) =  -41 
#> Effective size (Malpass, 1981, 
#>              adj Tredoux, 1998) =  -41 
#> [1] -41
esize_m(lineup_vec, 6)
#> [1] -41
```
