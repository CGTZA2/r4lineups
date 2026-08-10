# Descriptive statistics for bootstrapped lineup proportion

Function for computing the mean, median, and bootstrap standard error.
The standard deviation of the bootstrap replicates is the estimated
standard error; it is not divided by the square root of the number of
replicates.

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
#> Boot prop. (mean)   =  0.17997 
#> Boot prop. (median) =  0.18 
#> SD of boot prop     =  0.03838193 
#> SE of boot prop     =  0.038 
#> 2.5% boot CI lvl    =  0.11 
#> 97.5% boot CI lvl   =  0.26 

#OR:

lineuprops <- boot::boot(lineup_vec, lineup_prop_boot, target_pos = 3, R = 1000)
gen_boot_propmean_se(lineuprops$t)
#> Boot prop. (mean)   =  0.1814 
#> Boot prop. (median) =  0.18 
#> SD of boot prop     =  0.03957026 
#> SE of boot prop     =  0.04 
#> 2.5% boot CI lvl    =  0.11 
#> 97.5% boot CI lvl   =  0.26 
```
