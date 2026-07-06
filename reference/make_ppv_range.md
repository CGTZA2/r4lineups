# Main Function to Compute and Visualize PPV Range

Convenience wrapper that computes PPV range and creates visualizations.

## Usage

``` r
make_ppv_range(
  data,
  lineup_size = 6,
  confidence_bins = NULL,
  show_plots = TRUE,
  plot_type = "all"
)
```

## Arguments

- data:

  A dataframe with columns: target_present, identification, confidence

- lineup_size:

  Integer. Nominal number of people in lineup (default = 6)

- confidence_bins:

  Numeric vector of bin edges (optional)

- show_plots:

  Logical. Whether to create plots (default = TRUE)

- plot_type:

  Character. Which plots to create: "ppv" (PPV range only), "effective"
  (effective size only), "all" (all plots, default)

## Value

A list of class "lineup_ppv_range" containing:

- ppv_range_data: Dataframe with all PPV estimates

- plot_ppv_range: ggplot of PPV range (if requested)

- plot_effective_size: ggplot of effective size (if requested)

- plot_error_rate: ggplot of error rates (if requested)

- ... (additional components from ppv_range_by_confidence)

## Examples

``` r
data(lineup_example)
# With confidence binning
result <- make_ppv_range(lineup_example, confidence_bins = c(0, 60, 80, 100))

# Access plots
result$plot_ppv_range

result$plot_effective_size

```
