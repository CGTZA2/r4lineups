# Bias for each lineup member

Function to compute bias for each lineup member (assuming foil is
suspect, from Malpass, 1981)

## Usage

``` r
allfoilbias(lineup_table, target_pos, k)
```

## Arguments

- lineup_table:

  A table of lineup choices

- target_pos:

  A scalar, representing target position in lineup. Must be declared by
  user

- k:

  Nominal size (i.e., total number of members in lineup). Must be
  specified by user (scalar).

## References

Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
identification lineups. *Law and Human Behavior, 5*(4), 299-309.

## Examples

``` r
lineup_vec <- round(runif(100, 1, 6))
lineup_table <- table(lineup_vec)
x <- allfoilbias(lineup_table, 5, 6)
```
