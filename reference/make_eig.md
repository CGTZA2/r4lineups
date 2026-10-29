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
  lineup_size = 6,
  innocent_suspect = "auto"
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

- innocent_suspect:

  How innocent-suspect identifications in target-absent lineups are
  obtained. `"designated"`: the lineups had a designated innocent
  suspect, so target-absent `"suspect"` rows are counted directly, and
  zero such rows means a false-ID rate of zero. `"estimated"`: there was
  no designated innocent suspect, so the innocent-suspect count is
  estimated as filler IDs divided by `lineup_size`. `"auto"` (the
  default) chooses `"designated"` when any target-absent row is a
  suspect ID or the data have a non-missing `suspect_pos` column, and
  `"estimated"` otherwise, with a message.

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
