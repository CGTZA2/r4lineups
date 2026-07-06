# Lineup proportion for all lineup members

Computes lineup proportion for each member in a lineup

## Usage

``` r
allprop(lineup_vec, k)
```

## Arguments

- lineup_vec:

  A numeric vector of lineup choices

- k:

  Number of members in lineup. Must be specified by user (scalar).

## Value

Returns a vector containing lineup proportion for each lineup member

## References

Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
assessing the fairness of a lineup. *Law and Human Behavior, 3*(4),
285-293.

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1, 6))

#Call:
x <- allprop(lineup_vec, k = 6)
```
