# Lineup proportion over dataframe

Function for computing lineup proportion over a dataframe

## Usage

``` r
gen_lineup_prop(lineup_boot_df, target_pos, k)
```

## Arguments

- lineup_boot_df:

  Dataframe of lineup choices (usually a bootstrapped set)

- target_pos:

  A scalar, representing target position in lineup. Must be declared by
  user.

- k:

  A vector indexing number of members in each lineup pair (nominal
  size). Must be specified by user (scalar).

## Value

A vector of bootstrapped proportions, indicating the frequency with
which a target was identified in a lineup Length of vector = number of
bootstrap sample draws

## Examples

``` r
#Data
lineup_vec <- round(runif(100, 1, 6))
target_pos <- 3

#Get bootstrapped data:
lineup_boot_df <- gen_boot_samples(lineup_vec, 1000)

#Call:
lineuprops <- gen_lineup_prop(lineup_boot_df, 3, 6)
```
