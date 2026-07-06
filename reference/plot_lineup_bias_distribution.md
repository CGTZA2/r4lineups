# Plot Bootstrap Distribution of Lineup Bias

Plot Bootstrap Distribution of Lineup Bias

## Usage

``` r
plot_lineup_bias_distribution(boot_values, target_pos = NULL)
```

## Arguments

- boot_values:

  Numeric vector of bootstrap estimates.

- target_pos:

  Optional target position label for title.

## Value

A ggplot object.

## Examples

``` r
vec <- round(runif(200, 1, 6))
dist <- lineup_bias_boot_dist(vec, target_pos = 3, k = 6, R = 500)
plot_lineup_bias_distribution(dist, target_pos = 3)
```
