# Variance of ln of diagnosticity ratio

Function to compute variance of ln(d) for k lineup pairs

## Usage

``` r
var_lnd(linedf)
```

## Arguments

- linedf:

  A dataframe of parameters for computing diagnosticity ratio

## Value

A dataframe containing the estimated variance of the log diagnosticity
ratio for each lineup.

## Details

**To get linedf, use the diag_param helper function**

*diag_param* returns a dataframe containing the following:

- *n11*: Number of mock witnesses who identified the suspect in the
  target present condition

- *n21*: Number of mock witnesses who did not identify the suspect in
  the target present condition

- *n12*: Number of mock witnesses who identified the suspect in the
  target absent condition

- *n22*: Number of mock witnesses who did not identify the suspect in
  the target absent condition

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
#Target present data:
A <- rep(1:6, length.out = 100)
B <- rep(1:5, length.out = 70)
C <- rep(1:4, length.out = 20)
lineup_pres_list <- list(A, B, C)
rm(A, B, C)


#Target absent data:
A <- rep(6:1, length.out = 100)
B <- rep(5:1, length.out = 70)
C <- rep(4:1, length.out = 20)
lineup_abs_list <- list(A, B, C)
rm(A, B, C)

# Suspect position for each TP/TA pair
pos_list <- c(3, 2, 1)

#Nominal size:
k <- c(6, 5, 4)

#Use diag param helper function to get data (n11, n21, n12, n22):
linedf <- diag_param(lineup_pres_list, lineup_abs_list, pos_list, k)
#Call:
var <- var_lnd(linedf)
```
