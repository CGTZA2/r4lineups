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
as: (suspect IDs + filler IDs) / target-absent trials in that confidence
bin

The denominator is the number of target-absent trials within the bin, so
the value is the share of responses at that confidence that were
identifications. It is not a calibration measure and need not fall as
confidence rises: when rejections occur mainly at low confidence, the
rate is near 1 in the higher bins.

## Examples

``` r
data(lineup_example)
ppv_range <- ppv_range_by_confidence(lineup_example,
                                     confidence_bins = c(0, 60, 80, 100))
#> Target-absent lineups contain a designated innocent suspect, so innocent-suspect IDs are counted directly and no lineup-size correction is applied. The nominal, effective and uncorrected estimates are therefore identical for these data. The three diverge only when no innocent suspect is designated and the number of innocent-suspect IDs has to be estimated from the filler IDs.
plot_error_rate_conf(ppv_range)

```
