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

Information gain curves show the reduction in uncertainty (measured in
bits) for each response type across different prior probabilities.
Higher values indicate more diagnostic responses. Information gain is
maximized when the prior is near 0.5 (maximum uncertainty).

## Examples

``` r
data(lineup_example)
bayes <- make_bayes_curves(lineup_example)
plot_bayes_information_gain(bayes)

```
