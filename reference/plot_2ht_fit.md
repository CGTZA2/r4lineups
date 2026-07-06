# Plot Observed vs. Expected Counts

Creates a comparison plot showing observed counts vs. model-predicted
expected counts for both target-present and target-absent lineups.

## Usage

``` r
plot_2ht_fit(x, ...)
```

## Arguments

- x:

  A winter_2ht object from fit_winter_2ht()

- ...:

  Additional arguments (not used)

## Value

A ggplot object

## Examples

``` r
counts <- c(
  n_tp_suspect = 147, n_tp_filler = 94, n_tp_reject = 141,
  n_ta_suspect = 38, n_ta_filler = 138, n_ta_reject = 206
)
fit <- fit_winter_2ht(counts, lineup_size = 6)
plot_2ht_fit(fit)

```
