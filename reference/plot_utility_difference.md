# Plot Utility Difference Curve

Creates a plot showing the difference in expected utility between two
procedures across base rates.

## Usage

``` r
plot_utility_difference(util_diff_obj, show_crossover = TRUE)
```

## Arguments

- util_diff_obj:

  List output from make_utility_difference()

- show_crossover:

  Logical. Whether to mark crossover points. Default = TRUE.

## Value

A ggplot2 object

## Details

Utility difference curves reveal which procedure is superior at
different base rates. Regions above zero favor Procedure A, while
regions below zero favor Procedure B. Crossover points indicate base
rates where procedures are equivalent.

## Examples

``` r
# \donttest{
data(lineup_example)
odd <- seq(1, nrow(lineup_example), by = 2)
util_diff <- make_utility_difference(lineup_example[odd, ],
                                     lineup_example[-odd, ],
                                     base_rate_grid = seq(0.1, 0.9, 0.1))
plot_utility_difference(util_diff)

# }
```
