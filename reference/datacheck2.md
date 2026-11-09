# Helper function

Checks that number of lineup choices contained in a vector is accurate

## Usage

``` r
datacheck2(lineup_pres, lineup_abs, k1, k2)
```

## Arguments

- lineup_pres:

  A numeric vector of lineup choices for a lineup in which the target
  was present

- lineup_abs:

  A numeric vector of lineup choices for a lineup in which the target
  was absent

- k1:

  Number of members (nominal size) of the TP lineup

- k2:

  Number of members (nominal size) of the TA lineup

## Details

This function applies
[`datacheck1()`](https://cgtza2.github.io/r4lineups/reference/datacheck1.md)
to each lineup: every choice must be an integer position between 1 and
the lineup's `k`. It cannot detect a lineup member that no mock witness
chose, because such a member does not appear in the choices at all, so
`k1` and `k2` must be supplied correctly by the user.

## Examples

``` r
# Target-present and target-absent lineups of 6 members each
set.seed(1)
lineup_pres <- sample(1:6, 50, replace = TRUE)
lineup_abs <- sample(1:6, 50, replace = TRUE)
datacheck2(lineup_pres, lineup_abs, 6, 6)
```
