# Compare DPP Between Two Procedures

Computes and compares DPP for two lineup procedures.

## Usage

``` r
compare_dpp(data_proc_a, data_proc_b, lineup_size = 6)
```

## Arguments

- data_proc_a:

  Dataframe for procedure A (standard lineup format)

- data_proc_b:

  Dataframe for procedure B (standard lineup format)

- lineup_size:

  Integer. Lineup size (default = 6)

## Value

A list containing:

- dpp_a: DPP for procedure A

- dpp_b: DPP for procedure B

- dpp_difference: DPP_A - DPP_B (negative = A is better)

- dpp_obj_a: Full DPP object for procedure A

- dpp_obj_b: Full DPP object for procedure B

## Details

Compares two procedures using DPP. Since lower DPP indicates better
performance, a negative difference (DPP_A - DPP_B \< 0) means procedure
A is better.

DPP comparison is advantageous when:

- ROC curves have different truncation points

- Confidence distributions differ between procedures

- pAUC comparisons might be misleading

## Examples

``` r
data(lineup_example)
# Compare two halves of the example data (odd vs even rows)
odd <- seq(1, nrow(lineup_example), by = 2)
cmp <- compare_dpp(lineup_example[odd, ], lineup_example[-odd, ])
cmp$dpp_difference
#> [1] -0.006
```
