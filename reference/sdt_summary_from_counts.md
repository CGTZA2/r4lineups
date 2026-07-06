# SDT Summary Statistics from Counts

Compute d', criterion, and beta from 2x2 SDT counts (hits, false alarms,
misses, correct rejections).

## Usage

``` r
sdt_summary_from_counts(
  hits,
  fas,
  misses,
  cr,
  correction = c("loglinear", "half", "none")
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

- correction:

  Correction for extreme rates ("loglinear", "half", or "none").

## Value

A list with rates and SDT metrics.

## Examples

``` r
# 70 hits, 30 misses; 20 false alarms, 80 correct rejections
summ <- sdt_summary_from_counts(hits = 70, fas = 20, misses = 30, cr = 80)
summ$dprime
#> [1] 1.349772
summ$c
#> [1] 0.1561724
```
