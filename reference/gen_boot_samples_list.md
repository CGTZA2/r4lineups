# Bootstrapped resampling

Function for generating bootstrapped samples for of k vectors of lineup
choices

## Usage

``` r
gen_boot_samples_list(lineup_list, bootno)
```

## Arguments

- lineup_list:

  A list containing k vectors of lineup choices for k lineups, in which
  the target was either absent or present

- bootno:

  Number of bootstrap samples

## Value

A list of bootstrapped lineup data Length of list = number of bootstrap
sample draws

## Examples

``` r
#Data:
A <-  round(runif(100,1,6))
B <-  round(runif(70,1,5))
C <-  round(runif(20,1,4))
linelist <- list(A, B, C)
rm(A, B, C)

bootno <- 1000

#Call:
bootdata <- gen_boot_samples_list(linelist, bootno)
```
