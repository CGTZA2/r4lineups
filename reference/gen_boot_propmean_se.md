# Descriptive statistics for bootstrapped lineup proportion

Function for computing mean. med and se of boot proportion

## Usage

``` r
gen_boot_propmean_se(lineuprops)
```

## Arguments

- lineuprops:

  A dataframe of bootstrapped lineup proportions

## Value

Mean, median, standard deviation, standard error & 95 CIs of lineup
proportion across a bootstrapped dataframe

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1, 6))
target_pos <- 3

#Bootstrap data:
lineup_boot_df <- gen_boot_samples(lineup_vec, 1000)

#Compute proportion for bootstrap samples:
lineuprops <- gen_lineup_prop(lineup_boot_df, target_pos = 3, k = 6)

#Call:
gen_boot_propmean_se(lineuprops)
#> Boot prop. (mean)   =  0.18 
#> Boot prop. (median) =  0.18 
#> SD of boot prop     =  0.039 
#> SE of boot prop     =  0.001233288 
#> 2.5% boot CI lvl    =  0.11 
#> 97.5% boot CI lvl   =  0.26 

#OR:

lineuprops <- boot::boot(lineup_vec, lineup_prop_boot, target_pos = 3, R = 1000)
gen_boot_propmean_se(lineuprops$t)
#> Boot prop. (mean)   =  0.181 
#> Boot prop. (median) =  0.18 
#> SD of boot prop     =  0.04 
#> SE of boot prop     =  0.001264911 
#> 2.5% boot CI lvl    =  0.11 
#> 97.5% boot CI lvl   =  0.26 
```
