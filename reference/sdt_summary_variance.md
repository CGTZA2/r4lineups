# Variance Estimates for SDT Summary Metrics

Estimates the variance of d' (and criterion) from summary SDT counts
using the Gourevitch & Galanter (1967) delta method, the Miller (1996)
exact-binomial method, or a parametric bootstrap.

## Usage

``` r
sdt_summary_variance(
  hits,
  fas,
  misses,
  cr,
  method = c("miller", "gourevitch", "bootstrap"),
  correction = c("loglinear", "half", "none"),
  nboot = 1000,
  seed = NULL
)
```

## Arguments

- hits:

  Number of hits.

- fas:

  Number of false alarms.

- misses:

  Number of misses.

- cr:

  Number of correct rejections.

- method:

  Variance method: "gourevitch", "miller", or "bootstrap".

- correction:

  Correction for extreme rates ("loglinear", "half", or "none").

- nboot:

  Number of bootstrap samples when method = "bootstrap".

- seed:

  Optional random seed for bootstrap variance.

## Value

A list with variance estimates for zH, zF, d', and c.

## Examples

``` r
v <- sdt_summary_variance(hits = 70, fas = 20, misses = 30, cr = 80,
                          method = "miller")
v$var_dprime
#> [1] 0.03758609
```
