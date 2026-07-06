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

A list of bootstrapped lineup data Length of list = no. of boostrap
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
#> Warning: `rerun()` was deprecated in purrr 1.0.0.
#> ℹ Please use `map()` instead.
#>   # Previously
#>   rerun(1000, sample(lineup_list[[i]], length(lineup_list[[i]]), replace =
#>   TRUE))
#> 
#>   # Now
#>   map(1:1000, ~ sample(lineup_list[[i]], length(lineup_list[[i]]), replace =
#>   TRUE))
#> ℹ The deprecated feature was likely used in the r4lineups package.
#>   Please report the issue at <https://github.com/CGTZA2/r4lineups/issues>.
```
