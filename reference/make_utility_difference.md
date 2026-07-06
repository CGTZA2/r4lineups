# Compute Utility Difference Curves Comparing Two Procedures

Computes the difference in expected utility between two lineup
procedures across a range of base rates and/or utility matrices,
following Lampinen, Smith, & Wells (2019).

## Usage

``` r
make_utility_difference(
  data_proc_a,
  data_proc_b,
  base_rate_grid = seq(0.01, 0.99, 0.01),
  utility_matrix = c(tp = 1, fn = -0.5, fp = -2, tn = 0.5),
  utility_type = c("max", "avg", "all"),
  lineup_size = 6
)
```

## Arguments

- data_proc_a:

  A dataframe for procedure A (standard lineup format)

- data_proc_b:

  A dataframe for procedure B (standard lineup format)

- base_rate_grid:

  Numeric vector of base rates to evaluate. Default: seq(0.01, 0.99,
  0.01)

- utility_matrix:

  Named numeric vector with utilities (tp, fn, fp, tn). Default: c(tp=1,
  fn=-0.5, fp=-2, tn=0.5)

- utility_type:

  Character. Which utility to compare:

  - "max": Maximum utility (optimal criterion)

  - "avg": Average utility across all criteria

  - "all": Utility of accepting all identifications

  Default = "max".

- lineup_size:

  Integer. Lineup size (default = 6)

## Value

A list containing:

- difference_curve: Dataframe with base_rate, eu_a, eu_b, and difference

- utility_type: Type of utility compared

- utility_matrix: Utility matrix used

- crossover_points: Base rates where procedures have equal utility (if
  any)

## Details

Utility difference curves show how the relative value of two procedures
changes with the base rate (prevalence) of guilty suspects. This
addresses the question: "Which procedure is better, and does it depend
on the base rate?"

\$\$Utility Difference = EU_A - EU_B\$\$

Interpretation:

- Positive difference: Procedure A has higher utility (preferred)

- Negative difference: Procedure B has higher utility (preferred)

- Zero difference: Procedures equivalent at that base rate

Lampinen et al. (2019) show that ROC dominance does not guarantee
utility dominance. A procedure with a lower pAUC might have higher
utility at certain base rates and cost structures.

## References

Lampinen, J. M., Smith, A. M., & Wells, G. L. (2019). Four utilities in
eyewitness identification practice: Dissociations between receiver
operating characteristic analysis and expected utility analysis. *Law
and Human Behavior, 43*(1), 26-44.

## Examples

``` r
# \donttest{
data(lineup_example)
odd <- seq(1, nrow(lineup_example), by = 2)
util_diff <- make_utility_difference(lineup_example[odd, ],
                                     lineup_example[-odd, ],
                                     base_rate_grid = seq(0.1, 0.9, 0.1))
util_diff$difference_curve
#> # A tibble: 9 × 4
#>   base_rate  eu_a  eu_b difference
#>       <dbl> <dbl> <dbl>      <dbl>
#> 1       0.1 0.46  0.454    0.00600
#> 2       0.2 0.42  0.408    0.0120 
#> 3       0.3 0.38  0.362    0.0180 
#> 4       0.4 0.34  0.316    0.0240 
#> 5       0.5 0.329 0.27     0.0592 
#> 6       0.6 0.343 0.27     0.0733 
#> 7       0.7 0.358 0.302    0.055  
#> 8       0.8 0.372 0.335    0.0367 
#> 9       0.9 0.386 0.368    0.0183 
# }
```
