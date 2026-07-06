# Plot Bootstrap Distribution of Effective Size

Plot Bootstrap Distribution of Effective Size

## Usage

``` r
plot_esize_distribution(boot_values, metric = "Effective size")
```

## Arguments

- boot_values:

  Numeric vector of bootstrap estimates.

- metric:

  Label for the effective size statistic.

## Value

A ggplot object.

## Examples

``` r
vec <- round(runif(200, 1, 6))
dist <- esize_boot_dist(vec, k = 6, metric = "tredoux", R = 500)
plot_esize_distribution(dist, metric = "E (Tredoux, 1998)")
```
