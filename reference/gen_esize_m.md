# Effective Size (across a dataframe)

Function for computing Effective Size (Tredoux, 1998) on lineups
contained as columns in a df, usually from a bootstrapped sample

## Usage

``` r
gen_esize_m(lineup_boot_df, k)
```

## Arguments

- lineup_boot_df:

  A dataframe containing bootstrapped samples of lineup data

- k:

  Number of members in lineup. Must be specified by user (scalar).

## Value

A vector of effective size calculations for each lineup in bootstrapped
df

## Details

This function computes effective size for k lineups simultaneously.

## References

Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
identification lineups. *Law and Human Behavior, 5*(4), 299-309.

Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
& M. P. Toglia (Eds.), *Handbook of Eyewitness Psychology, Vol. 2:
Memory for people* (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum
Associates.

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22*(2), 217-237.

Tredoux, C. (1999). Statistical considerations when determining measures
of lineup size and lineup bias. *Applied Cognitive Psychology*, 13,
S9-S26.

Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
empirically assessing the fairness of a lineup. *Law and Human Behavior,
3*(4), 285-293.

## Examples

``` r
#Data:
lineup_vec <- round(runif(100,1,6))
bootdf <- gen_boot_samples(lineup_vec, 1000)

#Call:
esize_vec <- gen_esize_m(bootdf, 6)
```
