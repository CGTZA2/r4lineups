# Compare SDT Summary Metrics Between Two Conditions

Performs a large-sample z-test comparing d' (or criterion or ln beta)
between two independent conditions using summary counts only.

## Usage

``` r
compare_sdt_summary(
  hits_a,
  fas_a,
  misses_a,
  cr_a,
  hits_b,
  fas_b,
  misses_b,
  cr_b,
  metric = c("dprime", "c", "ln_beta"),
  method = c("miller", "gourevitch", "bootstrap"),
  correction = c("loglinear", "half", "none"),
  nboot = 1000,
  seed = NULL
)
```

## Arguments

- hits_a:

  Hits in condition A.

- fas_a:

  False alarms in condition A.

- misses_a:

  Misses in condition A.

- cr_a:

  Correct rejections in condition A.

- hits_b:

  Hits in condition B.

- fas_b:

  False alarms in condition B.

- misses_b:

  Misses in condition B.

- cr_b:

  Correct rejections in condition B.

- metric:

  Which metric to compare: "dprime", "c", or "ln_beta".

- method:

  Variance method: "miller", "gourevitch", or "bootstrap".

- correction:

  Correction for extreme rates ("loglinear", "half", or "none").

- nboot:

  Number of bootstrap samples when method = "bootstrap".

- seed:

  Optional random seed for bootstrap variance.

## Value

A list with estimates, standard error, z, and p-value.

## Examples

``` r
cmp <- compare_sdt_summary(hits_a = 70, fas_a = 20, misses_a = 30, cr_a = 80,
                           hits_b = 60, fas_b = 35, misses_b = 40, cr_b = 65,
                           metric = "dprime")
cmp$z
#> [1] 2.71054
cmp$p_value
#> [1] 0.006717369
```
