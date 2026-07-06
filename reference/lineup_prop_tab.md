# Lineup proportion

Computes the proportion of mock witnesses identifying a particular
lineup member

## Usage

``` r
lineup_prop_tab(lineup_table, target_pos)
```

## Arguments

- lineup_table:

  A table of lineup choices

- target_pos:

  A scalar, representing target position in lineup. Must be declared by
  user

## Value

Returns a proportion indicating the frequency with which a lineup member
was selected

## References

Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
empirically assessing the fairness of a lineup. *Law and Human Behavior,
3*(4), 285-293.

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1))
lineup_table <- table(lineup_vec)

#Call:
lineup_prop_tab(lineup_table, 3)
#> <NA> 
#>   NA 
lineup_prop_tab(table(lineup_vec), 2)
#> <NA> 
#>   NA 
```
