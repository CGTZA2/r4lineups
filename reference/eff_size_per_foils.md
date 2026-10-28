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

  Confidence level for the intervals, e.g. 0.95 for 95 percent
  intervals. Defaults to 0.95. May be specified by user (scalar).

- R:

  Number of bootstrap replications. Defaults to 1000.

## Value

A single integer: the number of lineup members whose bootstrapped
confidence interval for the proportion choosing them includes the chance
value \\1/k\\. These are the members that a mock witness chose at a rate
not distinguishable from guessing.

## Details

Because the result is a count of members rather than a continuous
quantity, it changes only in whole steps as `conf` varies, and it is
flat across ranges of `conf` in which no member's interval crosses
\\1/k\\. A value that does not move when `conf` is changed from, say,
0.95 to 0.99 is therefore expected behaviour and not a sign that the
argument is being ignored. Widening the interval can only ever increase
the count.

Intervals are computed by
[`lineup_boot_allprop`](https://cgtza2.github.io/r4lineups/reference/lineup_boot_allprop.md),
and are not corrected for the fact that `k` of them are examined at
once.

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
