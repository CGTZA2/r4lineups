# Confidence Intervals for Proportion

Function to compute ci high for each foil in a lineup

## Usage

``` r
allfoil_cihigh(linetabprops, sumlineup)
```

## Arguments

- linetabprops:

  A dataframe of bootstrapped lineup proportions

- sumlineup:

  Number of members in a lineup

## References

Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
identification lineups. *Law and Human Behavior, 5*(4), 299-309.

## Examples

``` r
# Upper CI bounds for the choice proportions of three lineup members,
# based on 20 mock witnesses
allfoil_cihigh(c(0.5, 0.3, 0.2), 20)
#> [1] 0.75 0.50 0.40
```
