# Compare DPP Between Two Procedures

Computes average DPP for two lineup procedures and tests their
difference with a nonparametric bootstrap, as Smith et al. (2019) did.

## Usage

``` r
compare_dpp(
  data_proc_a,
  data_proc_b,
  lineup_size = 6,
  alpha = NULL,
  beta = NULL,
  n_bootstrap = 1000,
  conf_level = 0.95,
  seed = NULL,
  innocent_suspect = "auto"
)
```

## Arguments

- data_proc_a:

  Dataframe for procedure A (standard lineup format)

- data_proc_b:

  Dataframe for procedure B (standard lineup format)

- lineup_size:

  Integer. Lineup size (default = 6)

- alpha, beta:

  Optional Equation 3 weights; see
  [`make_dpp`](https://cgtza2.github.io/r4lineups/reference/make_dpp.md).

- n_bootstrap:

  Integer. Number of bootstrap samples (default = 1000). Use 0 to skip
  the bootstrap.

- conf_level:

  Numeric. Confidence level for the interval (default = 0.95)

- seed:

  Integer. Random seed for reproducibility (default = NULL)

- innocent_suspect:

  How innocent-suspect identifications in target-absent lineups are
  obtained. `"designated"`: the lineups had a designated innocent
  suspect, so target-absent `"suspect"` rows are counted directly, and
  zero such rows means a false-ID rate of zero. `"estimated"`: there was
  no designated innocent suspect, so the innocent-suspect count is
  estimated as filler IDs divided by `lineup_size`. `"auto"` (the
  default) chooses `"designated"` when any target-absent row is a
  suspect ID or the data have a non-missing `suspect_pos` column, and
  `"estimated"` otherwise, with a message.

## Value

An object of class `"lineup_dpp_comparison"`, a list containing:

- dpp_a: Average DPP for procedure A

- dpp_b: Average DPP for procedure B

- dpp_difference: DPP_A - DPP_B (negative = A is better)

- se_difference: Bootstrap standard error of the difference

- ci_difference: Percentile bootstrap interval for the difference

- boot_differences: Bootstrap distribution of the difference

- dpp_obj_a, dpp_obj_b: Full
  [`make_dpp()`](https://cgtza2.github.io/r4lineups/reference/make_dpp.md)
  objects

- n_bootstrap, conf_level: Settings used

## Details

Lower DPP indicates better performance, so a negative difference means
procedure A is better. Each bootstrap replicate resamples target-present
and target-absent trials separately within each procedure, so every
replicate keeps the original design, and the two procedures are treated
as independent samples. The innocent-suspect design and the confidence
criteria are determined once on each full dataset and reused in every
replicate, so every replicate averages over the same criteria.

## References

Smith, A. M., Lampinen, J. M., Wells, G. L., Smalarz, L., &
Mackovichova, S. (2019). Deviation from perfect performance measures the
diagnostic utility of eyewitness lineups but partial area under the ROC
curve does not. *Journal of Applied Research in Memory and Cognition,
8*(1), 50-59.
[doi:10.1016/j.jarmac.2018.09.003](https://doi.org/10.1016/j.jarmac.2018.09.003)

## Examples

``` r
data(lineup_example)
# Compare two halves of the example data (odd vs even rows)
odd <- seq(1, nrow(lineup_example), by = 2)
# 50 resamples keep the example quick; use 1000 or more in an analysis
cmp <- compare_dpp(lineup_example[odd, ], lineup_example[-odd, ],
                   n_bootstrap = 50, seed = 1)
cmp
#> 
#> === DPP Comparison ===
#> 
#> Average DPP, procedure A:  0.6862 
#> Average DPP, procedure B:  0.7139 
#> Difference (A - B):        -0.0278  (negative = A better)
#> Bootstrap SE:              0.0310
#> 95% percentile interval:  [-0.0817, 0.0257]  (50 resamples)
```
