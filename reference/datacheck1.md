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

This function ensures that a non-selected lineup member is not
accidentally omitted from the dataframe due to lack of selection by all
mock witnesses. It functions as a check that the total number of lineup
members is accurate.

## Examples

``` r
# Choices of 50 mock witnesses to a 6-member lineup: passes silently
set.seed(1)
lineup_vec <- sample(1:6, 50, replace = TRUE)
datacheck1(lineup_vec, 6)
```
