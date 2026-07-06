# Plot Error Rate by Confidence Level

Creates a ggplot visualization showing the mistaken identification rate
(from target-absent lineups) across confidence levels.

## Usage

``` r
plot_error_rate_conf(ppv_range_obj, show_points = TRUE)
```

## Arguments

- ppv_range_obj:

  A lineup_ppv_range object from ppv_range_by_confidence(), or a
  dataframe with columns: confidence, error_rate

- show_points:

  Logical. Whether to show points at each confidence level (default =
  TRUE)

## Value

A ggplot2 object

## Details

The error rate is computed from target-absent (culprit-absent) lineups
as: (suspect IDs + filler IDs) / total target-absent trials

Higher confidence should generally be associated with lower error rates
(better calibration).

## Examples

``` r
data(lineup_example)
ppv_range <- ppv_range_by_confidence(lineup_example,
                                     confidence_bins = c(0, 60, 80, 100))
plot_error_rate_conf(ppv_range)

```
