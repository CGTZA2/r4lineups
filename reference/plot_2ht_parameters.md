# Plot Parameter Estimates from 2-HT Model

Creates a visual display of the parameter estimates from a fitted 2-HT
model, with error bars showing 95% confidence intervals.

## Usage

``` r
plot_2ht_parameters(x, ..., which = c("dP", "dA", "b", "g"), show_ci = TRUE)
```

## Arguments

- x:

  A winter_2ht object from fit_winter_2ht()

- ...:

  Additional arguments passed to plotting functions

- which:

  Character vector specifying which parameters to plot. Default is
  c("dP", "dA", "b", "g") (all parameters).

- show_ci:

  Logical indicating whether to show 95% confidence intervals. Default
  is TRUE.

## Value

A ggplot object

## Examples

``` r
counts <- c(
  n_tp_suspect = 147, n_tp_filler = 94, n_tp_reject = 141,
  n_ta_suspect = 38, n_ta_filler = 138, n_ta_reject = 206
)
fit <- fit_winter_2ht(counts, lineup_size = 6)
plot_2ht_parameters(fit)

```
