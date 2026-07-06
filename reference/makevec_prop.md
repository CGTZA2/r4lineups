# Helper functions

Several helper functions that compute bootcis from proportions

## Usage

``` r
makevec_prop(prop, n)
```

## Arguments

- prop:

  A dataframe of bootstrapped proportions

- n:

  Number of lineup members

## Examples

``` r
# Expand a choice proportion of 0.25 among 20 mock witnesses into a
# binary choice vector
makevec_prop(0.25, 20)
```
