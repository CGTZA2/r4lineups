# Main Function to Compute and Visualize EIG

Convenience wrapper that computes EIG and creates visualizations.

## Usage

``` r
make_eig(
  data,
  prior_guilt = 0.5,
  confidence_bins = NULL,
  show_plot = TRUE,
  plot_type = "both",
  lineup_size = 6
)
```

## Arguments

- data:

  A dataframe with columns: target_present, identification, confidence

- prior_guilt:

  Numeric. Prior probability that suspect is guilty (default = 0.5)

- confidence_bins:

  Numeric vector of bin edges (optional)

- show_plot:

  Logical. Whether to create plots (default = TRUE)

- plot_type:

  Character. Which plot to create: "ig" (information gain), "posteriors"
  (posterior probabilities), or "both" (default)

- lineup_size:

  Nominal lineup size used when no designated innocent suspect is
  present.

## Value

A list of class "lineup_eig" containing:

- eig: Expected Information Gain value

- response_data: Full response category data

- plot_ig: ggplot of information gain (if requested)

- plot_posteriors: ggplot of posteriors (if requested)

- ... (additional components from compute_eig)

## Examples

``` r
data(lineup_example)
# With confidence binning
result <- make_eig(lineup_example, confidence_bins = c(0, 60, 80, 100))

# Access plots
result$plot_ig

result$plot_posteriors

```
