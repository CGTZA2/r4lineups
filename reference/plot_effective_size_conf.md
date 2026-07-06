# Plot Effective Size by Confidence Level

Creates a ggplot visualization showing how effective lineup size varies
across confidence levels.

## Usage

``` r
plot_effective_size_conf(
  ppv_range_obj,
  show_nominal = TRUE,
  show_points = TRUE
)
```

## Arguments

- ppv_range_obj:

  A lineup_ppv_range object from ppv_range_by_confidence(), or a
  dataframe with columns: confidence, effective_size

- show_nominal:

  Logical. Whether to show nominal lineup size as reference line
  (default = TRUE)

- show_points:

  Logical. Whether to show points at each confidence level (default =
  TRUE)

## Value

A ggplot2 object

## Details

Effective size \< nominal size indicates lineup bias, where some members
are implausible and rarely chosen. This can vary by confidence level if
witnesses at different confidence levels have different lineup viewing
strategies.

A horizontal line at the nominal lineup size provides a reference.
Values below this line indicate bias; values at or above indicate fair
lineups.

## Examples

``` r
data(lineup_example)
ppv_range <- ppv_range_by_confidence(lineup_example,
                                     confidence_bins = c(0, 60, 80, 100))
plot_effective_size_conf(ppv_range)

```
