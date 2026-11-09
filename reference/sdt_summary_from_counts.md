# SDT Summary Statistics from Counts

Compute d', criterion, and beta from 2x2 SDT counts (hits, false alarms,
misses, correct rejections). For lineup data, a hit is a correct
identification of the culprit in a target-present lineup and a false
alarm is an identification of the designated innocent suspect in a
target-absent lineup; the yes/no model is then an approximation (see
[`vignette("sdt_summary_comparisons")`](https://cgtza2.github.io/r4lineups/articles/sdt_summary_comparisons.md)).

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

  Correction for extreme rates ("loglinear", "half", or "none"). The
  default "loglinear" adjusts every rate, \\(x + 0.5)/(n + 1)\\, not
  only extreme ones; "half" adjusts only counts of 0 or n; "none" leaves
  rates unadjusted, so a rate of 0 or 1 gives infinite estimates.

## Value

A list with rates and SDT metrics. The returned hit and false-alarm
rates are the adjusted rates.

## Examples

``` r
# 70 hits, 30 misses; 20 false alarms, 80 correct rejections
summ <- sdt_summary_from_counts(hits = 70, fas = 20, misses = 30, cr = 80)
summ$dprime
#> [1] 1.349772
summ$c
#> [1] 0.1561724
```
