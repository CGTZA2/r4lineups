# Lineup vector

A function that takes a table of lineup choices and generates a vector
of raw lineup data

## Usage

``` r
gen_linevec(lineup_table, k)
```

## Arguments

- lineup_table:

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
lineup_table <- table(lineup_vec)

#Call:
lineup_vec <- gen_linevec(lineup_table, 3)
```
