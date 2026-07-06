# Plot Expected Utility Curves

Creates a plot showing expected utility across confidence criteria.

## Usage

``` r
plot_utility_curves(utility_obj, show_max = TRUE, show_reject = TRUE)
```

## Arguments

- utility_obj:

  List output from make_utility_curves()

- show_max:

  Logical. Whether to highlight maximum utility point. Default = TRUE.

- show_reject:

  Logical. Whether to show reject-all utility point. Default = TRUE.

## Value

A ggplot2 object

## Details

The utility curve shows the expected utility at each confidence
criterion. Higher confidence criteria have lower hit rates but also
lower false alarm rates. The optimal criterion maximizes expected
utility given the base rate and cost structure.

## Examples

``` r
data(lineup_example)
util <- make_utility_curves(lineup_example)
plot_utility_curves(util)

```
