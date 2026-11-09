# Helper function

Checks that number of lineup choices contained in a vector is accurate

## Usage

``` r
datacheck1(lineup_vec, k)
```

## Arguments

- lineup_vec:

  A numeric vector of lineup choices

- k:

  Number of members in lineup

## Details

This function checks that every choice is an integer position between 1
and `k`. It cannot detect a lineup member that no mock witness chose,
because such a member does not appear in the choices at all, so `k` must
be supplied correctly by the user.
[`lineup_table()`](https://cgtza2.github.io/r4lineups/reference/lineup_table.md)
builds a table with a cell for every member.

## Examples

``` r
# Choices of 50 mock witnesses to a 6-member lineup: passes silently
set.seed(1)
lineup_vec <- sample(1:6, 50, replace = TRUE)
datacheck1(lineup_vec, 6)
```
