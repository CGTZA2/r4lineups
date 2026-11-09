# Helper function

Checks that number of lineup choices contained in a data table is
accurate

## Usage

``` r
datacheck3(lineup_tab, k)
```

## Arguments

- lineup_tab:

  A table of lineup choices

- k:

  Nominal size (i.e., total number of members in lineup)

## Details

This function checks that the table has no more than `k` cells, that its
counts are finite and non-negative, and that its names (if any) are
integer positions between 1 and `k`. An unnamed table must have exactly
`k` cells. A named table with fewer than `k` positions, such as
[`table()`](https://rdrr.io/r/base/table.html) of choices in which one
member was never chosen, is accepted, because the missing member cannot
be detected from the table, so `k` must be supplied correctly by the
user.
[`lineup_table()`](https://cgtza2.github.io/r4lineups/reference/lineup_table.md)
builds a table with a cell for every member.

## Examples

``` r
# Table of choices from 50 mock witnesses to a 6-member lineup
set.seed(1)
lineup_tab <- table(sample(1:6, 50, replace = TRUE))
datacheck3(lineup_tab, 6)
```
