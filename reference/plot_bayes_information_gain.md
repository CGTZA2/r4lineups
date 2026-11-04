# Plot Information Gain Curves

Creates a plot showing how much uncertainty is reduced by different
lineup responses.

## Usage

``` r
plot_bayes_information_gain(bayes_obj, selected_responses = NULL)
```

## Arguments

- bayes_obj:

  List output from make_bayes_curves()

- selected_responses:

  Character vector of response categories to plot. If NULL (default),
  plots all responses.

## Value

A ggplot2 object

## Details

The vertical axis follows the `gain` used in
[`make_bayes_curves()`](https://cgtza2.github.io/r4lineups/reference/make_bayes_curves.md).
With `gain = "probability"` (Wells et al., 2015) it is posterior minus
prior: positive for responses that point to guilt, negative for
responses that point to innocence. With `gain = "entropy"` it is the
reduction in uncertainty in bits: a response that points to guilt gains
most at priors above 0.5 and one that points to innocence at priors
below 0.5, and either can be negative where it moves belief towards 0.5.
The legend lists responses from the most to the least indicative of
guilt.

## Examples

``` r
data(lineup_example)
bayes <- make_bayes_curves(lineup_example)
plot_bayes_information_gain(bayes)

```
