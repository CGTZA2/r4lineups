# Diagnosticity ratio weights

Function to compute weights of each diagnosticity ratio for k lineup
pairs

## Usage

``` r
d_weights(linedf)
```

## Arguments

- linedf:

  A dataframe of parameters for computing diagnosticity ratio

## Value

A dataframe with one column containing weights for each pair of lineups
for which diagnosticity ratio is being calculated.

## Details

In order to obtain a pooled estimate of a set of diagnosticity ratios,
we use a weight for each ratio that is equal to the inverse of its
variance.

**To get linedf, use the diag_param helper function**

*diag_param* returns a dataframe containing the following:

- *n11*: Number of mock witnesses who identified the suspect in the
  target present condition

- *n21*: Number of mock witnesses who did not identify the suspect in
  the target present condition

- *n12*: Number of mock witnesses who identified the suspect in the
  target absent condition

- *n13*: Number of mock witnesses who did not identify the suspect in
  the target absent condition

## References

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22*(2), 217-237.

## Examples

``` r
#Target present data:
A <-  round(runif(100,1,6))
B <-  round(runif(70,1,5))
C <-  round(runif(20,1,4))
lineup_pres_list <- list(A, B, C)
rm(A, B, C)


#Target absent data:
A <-  round(runif(100,1,6))
B <-  round(runif(70,1,5))
C <-  round(runif(20,1,4))
lineup_abs_list <- list(A, B, C)
rm(A, B, C)

#Pos list
lineup1_pos <- c(1, 2, 3, 4, 5, 6)
lineup2_pos <- c(1, 2, 3, 4, 5)
lineup3_pos <- c(1, 2, 3, 4)
pos_list <- list(lineup1_pos, lineup2_pos, lineup3_pos)
rm(lineup1_pos, lineup2_pos, lineup3_pos)

#Nominal size:
k <- c(6, 5, 4)

#Use diag param helper function to get data (n11, n21, n12, n22):
linedf <- diag_param(lineup_pres_list, lineup_abs_list, pos_list, k)
#> Warning: longer object length is not a multiple of shorter object length
#> Warning: longer object length is not a multiple of shorter object length
#> Warning: longer object length is not a multiple of shorter object length
#> Warning: longer object length is not a multiple of shorter object length

#Call:
wi <- d_weights(linedf)
```
