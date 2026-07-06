# Plot DPP with Observed and Perfect ROC Curves

Creates a plot showing the observed ROC curve and perfect ROC curve,
with shaded area representing deviation from perfect performance.

## Usage

``` r
plot_dpp(dpp_obj, show_perfect = TRUE, show_dpp = TRUE, show_shading = TRUE)
```

## Arguments

- dpp_obj:

  List output from make_dpp()

- show_perfect:

  Logical. Whether to show perfect ROC curve (default = TRUE)

- show_dpp:

  Logical. Whether to display DPP value on plot (default = TRUE)

- show_shading:

  Logical. Whether to shade area between curves (default = TRUE)

## Value

A ggplot2 object

## Details

The plot shows:

- **Observed ROC** (blue): Actual performance

- **Perfect ROC** (red dashed): Best achievable performance

- **Shaded area** (red): Deviation from perfect (area = DPP ×
  AUC_perfect)

- **DPP value**: Proportion of perfect area that is lost

A smaller shaded area (lower DPP) indicates better performance.

## Examples

``` r
data(lineup_example)
dpp <- make_dpp(lineup_example)
plot_dpp(dpp)

```
