# Compare ANRI Between Groups with Bootstrap

Computes ANRI for two groups and tests whether they differ using
bootstrap confidence intervals for the difference.

## Usage

``` r
compare_anri(
  data,
  group_var,
  confidence_bins,
  choosers_only = TRUE,
  lineup_size = 6,
  n_bootstrap = 1000,
  conf_level = 0.95,
  seed = NULL,
  confidence_scale = c("auto", "0-1", "0-100")
)
```

## Arguments

- data:

  Dataframe with standard lineup format plus a grouping variable

- group_var:

  Character. Name of grouping variable (must have exactly 2 levels)

- confidence_bins:

  Numeric vector of bin edges

- choosers_only:

  Logical. Whether to analyze only suspect IDs (default = TRUE)

- lineup_size:

  Integer. Lineup size (default = 6)

- n_bootstrap:

  Integer. Number of bootstrap replications (default = 1000)

- conf_level:

  Numeric. Confidence level for CIs (default = 0.95)

- seed:

  Integer. Random seed for reproducibility (default = NULL)

- confidence_scale:

  How the confidence scale is interpreted: "auto" (default), "0-1", or
  "0-100". See
  [`make_calibration_data`](https://cgtza2.github.io/r4lineups/reference/make_calibration_data.md).

## Value

A list containing:

- anri_group1: ANRI for first group

- anri_group2: ANRI for second group

- difference: Point estimate of difference (group1 - group2)

- ci_lower: Lower bound of CI for difference

- ci_upper: Upper bound of CI for difference

- significant: Whether difference is significant (CI excludes 0)

- group_names: Names of the two groups

- bootstrap_results_group1: Bootstrap object for group 1

- bootstrap_results_group2: Bootstrap object for group 2

## Details

This function: 1. Computes ANRI separately for each group 2. Bootstraps
each group independently 3. Computes bootstrap distribution of the
difference 4. Tests H0: ANRI_1 = ANRI_2 using CI for difference

\*\*Interpretation\*\*:

- Positive difference: Group 1 has higher resolution

- Negative difference: Group 2 has higher resolution

- CI excludes 0: Statistically significant difference

## Examples

``` r
# \donttest{
set.seed(123)
n <- 300
data <- data.frame(
  target_present = rep(c(TRUE, FALSE), each = n / 2),
  identification = sample(c("suspect", "filler", "reject"), n,
                          replace = TRUE, prob = c(0.5, 0.25, 0.25)),
  confidence = round(runif(n, 0, 100)),
  condition = rep(c("sequential", "simultaneous"), times = n / 2)
)
cmp <- compare_anri(data, group_var = "condition",
                    confidence_bins = seq(0, 100, 20),
                    n_bootstrap = 100, seed = 1)
cmp$difference
#> [1] -0.008229687
c(cmp$ci_lower, cmp$ci_upper)
#> [1] -0.1783907  0.1099995
# }
```
