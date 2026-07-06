# Plot PPV Range Across Confidence Levels

Creates a ggplot visualization showing the range of PPV estimates across
the three correction methods (nominal, effective, none) as recommended
by Fitzgerald et al. (2023).

## Usage

``` r
plot_ppv_range(ppv_range_obj, show_band = TRUE, show_points = TRUE)
```

## Arguments

- ppv_range_obj:

  A lineup_ppv_range object from ppv_range_by_confidence()

- show_band:

  Logical. Whether to show shaded uncertainty band between nominal and
  none estimates (default = TRUE)

- show_points:

  Logical. Whether to show points at each confidence level (default =
  TRUE)

## Value

A ggplot2 object

## Details

The plot shows three curves:

- **Nominal** (blue, dashed): Best-case estimate assuming fair lineup

- **Effective** (red, solid): Realistic estimate accounting for bias

- **None** (gray, dashed): Worst-case estimate (no correction)

The shaded band between nominal and none represents the PPV uncertainty
range due to unknown lineup fairness conditions. The effective size
estimate typically falls within this range and provides a more realistic
middle ground.

## References

Fitzgerald, R. J., Tredoux, C. G., & Juncu, S. (2023). Estimation of
eyewitness error rates in fair and biased lineups. *Law and Human
Behavior*.

## Examples

``` r
data(lineup_example)
ppv_range <- ppv_range_by_confidence(lineup_example,
                                     confidence_bins = c(0, 60, 80, 100))
plot_ppv_range(ppv_range)

```
