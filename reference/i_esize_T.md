# I Component of Tredoux Effective Size (Tredoux, 1998)

Computes the Agresti-Agresti index of diversity I, the intermediate
quantity used to construct Tredoux's effective size E' = 1/(1 - I).

## Usage

``` r
i_esize_T(lineup_table)
```

## Arguments

- lineup_table:

  A table of lineup choices.

## Value

A scalar: the diversity index I.

## Details

Tredoux (1998) used Agresti and Agresti's (1978) index of diversity:
\$\$I = 1 - \frac{\sum_i o_i^2}{N^2},\$\$ where \\o_i\\ is the observed
count for lineup member \\i\\ and \\N\\ is the total number of choices.
This equals \\1 - \sum_i p_i^2\\, where \\p_i = o_i/N\\. The effective
size is the reciprocal transform \\E' = 1/(1-I)\\, computed by
[`esize_T`](https://cgtza2.github.io/r4lineups/reference/esize_T.md).

## References

Agresti, A., & Agresti, B. F. (1978). Statistical analysis of
qualitative variation. *Sociological Methodology, 9*, 204-237.

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22*(2), 217-237.

## See also

[`esize_T`](https://cgtza2.github.io/r4lineups/reference/esize_T.md)

## Examples

``` r
lineup_vec <- round(runif(100, 1, 6))
lineup_table <- table(lineup_vec)
i <- i_esize_T(lineup_table)
```
