# Plot CAC Curve

Creates a ggplot2 visualization of CAC data showing the relationship
between confidence and accuracy.

## Usage

``` r
make_cac_gg(cacobj_list, show_errorbars = TRUE, show_n = TRUE)
```

## Arguments

- cacobj_list:

  List output from make_cacdata()

- show_errorbars:

  Logical. Whether to show error bars (default = TRUE)

- show_n:

  Logical. Whether to show sample sizes (default = TRUE)

## Value

A ggplot2 object

## Examples

``` r
data(lineup_example)
cac <- make_cacdata(lineup_example, confidence_bins = c(0, 60, 80, 100))
make_cac_gg(cac)

```
