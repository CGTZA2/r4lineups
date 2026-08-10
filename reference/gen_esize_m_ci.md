# Bootstrap Quantile for Effective Size

Computes one requested quantile from a vector of bootstrapped Malpass
effective sizes. Call the function twice (for example, at 0.025 and
0.975) to obtain both endpoints of a percentile interval.

## Usage

``` r
gen_esize_m_ci(lineupsizes, perc = 0.05)
```

## Arguments

- lineupsizes:

  A non-empty numeric vector of bootstrapped effective sizes.

- perc:

  A single quantile probability between 0 and 1. Defaults to 0.05.

## Value

A single named bootstrap quantile.

## Examples

``` r
#Data:
lineup_vec <- rep(1:6, length.out = 100)
k <- 6

#Use gen_boot_samples to get bootstrapped data:
bootdata <- gen_boot_samples(lineup_vec, 1000)

#Compute effective size over df of bootstrapped data:
lineupsizes <- gen_esize_m(bootdata, 6)

#Call:
gen_esize_m_ci(lineupsizes)
#>   5% 
#> 5.16 
gen_esize_m_ci(lineupsizes, perc = .025)
#> 2.5% 
#>  5.1 
gen_esize_m_ci(lineupsizes, perc = .975)
#> 97.5% 
#>  5.76 
```
