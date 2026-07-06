# Parameters for diagnosticity ratio

This function calculates the parameters needed to calculate the
diagnosticity ratio for several lineup pairs.

## Usage

``` r
diag_param(lineup_pres_list, lineup_abs_list, pos_list, k)
```

## Arguments

- lineup_pres_list:

  A list containing k vectors of lineup choices for k lineups, in which
  the target was present

- lineup_abs_list:

  A list containing k vectors of lineup choices for k lineups, in which
  the target was absent

- pos_list:

  A list containing k numeric vectors indexing lineup member positions
  for each lineup pair

- k:

  A vector indexing number of members in each lineup pair. Must be
  specified by user (scalar).

## Value

Returns a dataframe containing:

- *n11*: Number of mock witnesses who identified the suspect in the
  target present condition

- *n21*: Number of mock witnesses who did not identify the suspect in
  the target present condition

- *n12*: Number of mock witnesses who identified the suspect in the
  target absent condition

- *n13*: Number of mock witnesses who did not identify the suspect in
  the target absent condition

## Details

- Lineup pairs consist of one lineup in which the target was present
  (TP) and one lineup in which the target was absent (TA).

- Each lineup pair must occupy corresponding positions in the TA and TP
  lists.

  Example:

  For a lineup pair A that consists of (1)TP lineup and (2)TA lineup:
  A(1) is the first vector in the TP list A(2) is the first vector in
  the TP list

- The order in which nominal size for each lineup pair is listed must
  also correspond with the positions of each respective lineup in the
  lineup lists (i.e., if lineup 1 has k = 6, then the first element of
  vector 'k' = 6)

- Data must be in a list format. This allows the function to compare
  lineups in which the number of choices and number of lineup members
  differs.

- The following warning will appear if vectors comprising lineup lists
  are of different lengths: *longer object length is not a multiple of
  shorter object length*. **This does not affect the accuracy of the
  function and can be ignored.**

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

#Call:
linedf <- diag_param(lineup_pres_list, lineup_abs_list, pos_list, k)
#> Warning: longer object length is not a multiple of shorter object length
#> Warning: longer object length is not a multiple of shorter object length
#> Warning: longer object length is not a multiple of shorter object length
#> Warning: longer object length is not a multiple of shorter object length
```
