# Lineup vector

A function that takes a table of lineup choices and generates a vector
of raw lineup data

## Usage

``` r
gen_linevec(lineup_tab, k)
```

## Arguments

- lineup_tab:

  A table of lineup choices

- k:

  Nominal size (i.e.,total number of lineup members). Must be declared
  by user

## Value

Returns a vector of lineup choices

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1, 6))
lineup_tab <- table(lineup_vec)

#Call:
lineup_vec <- gen_linevec(lineup_tab, 6)
```
