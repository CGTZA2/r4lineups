# Jackknife Acceleration for 2-HT BCa Intervals

Leaving out one observation changes one of the six response counts by
one, so there are at most six distinct leave-one-out fits. Each is
weighted by the number of observations in its cell (Efron, 1987).

## Usage

``` r
.jackknife_acceleration_2ht(original_fit, ...)
```

## Arguments

- original_fit:

  A winter_2ht object

- ...:

  Additional arguments passed to fit_winter_2ht()

## Value

Named vector of acceleration constants, one per parameter
