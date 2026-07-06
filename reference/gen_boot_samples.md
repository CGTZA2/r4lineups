# Bootstrap resampling

Function for generating bootstrapped samples from 1 vector of lineup
data

## Usage

``` r
gen_boot_samples(lineup_vec, bootno)
```

## Arguments

- lineup_vec:

  A numeric vectors of lineup choices

- bootno:

  Number of bootstrap samples

## Value

A dataframe of bootstrapped lineup data

## Examples

``` r
#Data:
lineup_vec <- round(runif(100,1,6))
bootno <- 1000

#Call:
bootdf <- gen_boot_samples(lineup_vec, bootno)
```
