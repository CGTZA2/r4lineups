# Lineup proportion

Computes the proportion of mock witnesses identifying a particular
lineup member

## Usage

``` r
lineup_prop_vec(lineup_vec, target_pos, k)
```

## Arguments

- lineup_vec:

  A numeric vector of lineup choices

- target_pos:

  Suspect/lineup member position. Must be declared by user (scalar).

- k:

  Nominal size (i.e., total number of members in lineup). Must be
  specified by user (scalar).

## Value

Returns a proportion indicating the frequency with which a target was
identified in a lineup

## References

Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
empirically assessing the fairness of a lineup. *Law and Human Behavior,
3*(4), 285-293.

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1, 6))

#Call:
lineup_prop_vec(lineup_vec, 3, 6)
#> [1] 0.2
```
