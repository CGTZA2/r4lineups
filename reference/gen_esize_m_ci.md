# Bootstrapped Confidence Intervals for Effective Size

Function for computing Effective Size (Malpass, 1981, as adjusted by
Tredoux, 1998) with CIs from bootstrap df of lineups

## Usage

``` r
gen_esize_m_ci(lineupsizes, perc = 0.05)
```

## Arguments

- lineupsizes:

  A vector of bootstrapped effective sizes

- perc:

  Defaults to .05. Can be specified by user, according to desired level
  of alpha (scalar)

## Value

Confidence intervals for effective size

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1, 6))
k <- 6

#Use gen_boot_samples to get bootstrapped data:
bootdata <- gen_boot_samples(lineup_vec, 1000)

#Compute effective size over df of bootstrapped data:
lineupsizes <- gen_esize_m(bootdata, 6)

#Call:
gen_esize_m_ci(lineupsizes)
#>    5% 
#> 4.679 
gen_esize_m_ci(lineupsizes, perc = .025)
#> 2.5% 
#> 4.58 
gen_esize_m_ci(lineupsizes, perc = .975)
#> 97.5% 
#>  5.44 
```
