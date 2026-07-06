# Plot Posterior Probabilities for Response Categories

Creates a ggplot2 visualization showing how each response category
updates beliefs about guilt (posterior probabilities).

## Usage

``` r
plot_eig_posteriors(eig_obj, max_responses = 15, show_prior = TRUE)
```

## Arguments

- eig_obj:

  A lineup_eig object from compute_eig()

- max_responses:

  Maximum number of response categories to display (default = 15)

- show_prior:

  Logical. Whether to show prior probability line (default = TRUE)

## Value

A ggplot2 object

## Details

This function creates a bar plot showing the posterior probability of
guilt for each response category. The prior probability is shown as a
dashed line for reference. Response categories that push beliefs toward
guilt (posterior \> prior) are colored red, while those pushing toward
innocence are colored blue.

## Examples

``` r
data(lineup_example)
eig_result <- compute_eig(lineup_example, confidence_bins = c(0, 60, 80, 100))
plot_eig_posteriors(eig_result)

```
