# Plot Prior-Posterior Curves

Creates a plot showing how different lineup responses update prior
beliefs about suspect guilt to posterior beliefs.

## Usage

``` r
plot_bayes_prior_posterior(
  bayes_obj,
  selected_responses = NULL,
  show_diagonal = TRUE
)
```

## Arguments

- bayes_obj:

  List output from make_bayes_curves()

- selected_responses:

  Character vector of response categories to plot. If NULL (default),
  plots all responses.

- show_diagonal:

  Logical. Whether to show diagonal line (no update). Default = TRUE.

## Value

A ggplot2 object

## Details

Prior-posterior curves show Bayesian updating for different responses.
The diagonal line represents no update (posterior = prior). Curves above
the diagonal indicate responses that increase belief in guilt, while
curves below decrease belief in guilt.

## Examples

``` r
data(lineup_example)
bayes <- make_bayes_curves(lineup_example)
plot_bayes_prior_posterior(bayes)

```
