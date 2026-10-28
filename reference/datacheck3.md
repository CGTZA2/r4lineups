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

This function ensures that a non-selected lineup member is not
accidentally omitted from the data due to lack of selection by all mock
witnesses. It functions as a check that the total number of lineup
members is accurate.

## Examples

``` r
# Table of choices from 50 mock witnesses to a 6-member lineup
set.seed(1)
lineup_tab <- table(sample(1:6, 50, replace = TRUE))
datacheck3(lineup_tab, 6)
```
