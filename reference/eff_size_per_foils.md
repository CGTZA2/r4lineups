# Effective Size per Foils

Function for computing effective size (Malpass, 1981) counting foils who
fall within the CI for chance guessing

## Usage

``` r
eff_size_per_foils(lineup_vec, target_pos, k, conf = 0.95, R = 1000)
```

## Arguments

- lineup_vec:

  A numeric vector of lineup choices

- target_pos:

  A numeric vector indexing all lineup members

- k:

  Number of members in lineup. Must be specified by user (scalar).

- conf:

  Desired level of alpha. Defaults to 0.95. May be specified by user
  (scalar).

- R:

  Number of bootstrap replications. Defaults to 1000.

## References

Malpass, R S. (1981). Effective size and defendant bias in eyewitness
identification lineups. *Law and Human Behavior, 5*, 299-309.

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22*(2), 217-237.

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1, 6))
target_pos <- c(1, 2, 3, 4, 5, 6)

#Call:
eff_size_per_foils(lineup_vec, target_pos, 6)
#> [1] 5
eff_size_per_foils(lineup_vec, target_pos, conf = 0.95, 6)
#> [1] 5
```
