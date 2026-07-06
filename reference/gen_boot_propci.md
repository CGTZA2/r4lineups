# Percentile of Bootstrapped Lineup Proportion

Function for computing arbitrary percentile of bootstrapped lineup
proportion

## Usage

``` r
gen_boot_propci(lineuprops, perc = 0.05)
```

## Arguments

- lineuprops:

  A vector of bootstrapped lineup proportions

- perc:

  Percentile to be computed. Must be declared by user (scalar)

  Defaults to .05

## Value

Arbitrary percentile of bootstrapped lineup proportion

## Details

Can be used to calculate confidence intervals at desired level of alpha

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1, 6))
target_pos <- 3
k <- 6

#Bootstrap data:
lineup_boot_df <- gen_boot_samples(lineup_vec, 1000)
#> Warning: `rerun()` was deprecated in purrr 1.0.0.
#> ℹ Please use `map()` instead.
#>   # Previously
#>   rerun(1000, sample(lineup_vec, length(lineup_vec), replace = TRUE))
#> 
#>   # Now
#>   map(1:1000, ~ sample(lineup_vec, length(lineup_vec), replace = TRUE))
#> ℹ The deprecated feature was likely used in the r4lineups package.
#>   Please report the issue at <https://github.com/CGTZA2/r4lineups/issues>.

#Compute proportion for bootstrap samples:
lineuprops <- gen_lineup_prop(lineup_boot_df, target_pos, 6)

#Get boot CIs:
prop_bootci_lower <- gen_boot_propci(lineuprops, perc = .025)
prop_bootci_upper <- gen_boot_propci(lineuprops, perc = .975)
```
